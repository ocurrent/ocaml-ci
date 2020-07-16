open Current.Syntax
open Capnp_rpc_lwt
open Lwt.Infix

let src = Logs.Src.create "ocaml_ci.cluster_build" ~doc:"ocaml-ci ocluster builder"
module Log = (val Logs.src_log src : Logs.LOG)

module Metrics = struct
  open Prometheus

  let namespace = "ocamlci"
  let subsystem = "ocluster"

  let queue =
    let help = "Items in cluster queue by state" in
    Gauge.v_label ~label_name:"state" ~help ~namespace ~subsystem "queue_state_total"

  let queue_connect = queue "connect"
  let queue_commit_lock = queue "commit-lock"
  let queue_rate_limit = queue "rate-limit"
  let queue_get_ticket = queue "get-ticket"
  let queue_get_worker = queue "get-worker"
end

module Selection = Ocaml_ci_api.Worker.Selection
module Image = Current_docker.Raw.Image
module Git = Current_git

let ( >>!= ) = Lwt_result.bind

(* Make sure we never build the same (commit, variant) twice at the same time, as this is likely
   to trigger BuildKit bug https://github.com/moby/buildkit/issues/1456.
   While a build is in progress, this contains a promise for the build finishing. *)
let commit_locks = Hashtbl.create 1000

let with_commit_lock ~job commit variant fn ~priority ~switch =
  let cancel = ref Lwt.return in
  let rec aux () =
    let key = (Current_git.Commit_id.hash commit, variant) in
    match Hashtbl.find_opt commit_locks key with
    | Some lock ->
      Current.Job.log job "Waiting for a similar build to finish...";
      let cancelled, set_cancelled = Lwt.wait () in
      cancel := (fun () ->
          if Lwt.is_sleeping cancelled then (
            Current.Job.log job "Cancelling with_commit_lock";
            Lwt.wakeup_exn set_cancelled Lwt.Canceled;
          );
          Lwt.return_unit
        );
      Prometheus.Gauge.inc_one Metrics.queue_commit_lock;
      Lwt.finalize
        (fun () -> Lwt.choose [lock; cancelled])
        (fun () -> Prometheus.Gauge.dec_one Metrics.queue_commit_lock; Lwt.return_unit)
      >>= aux
    | None ->
      let finished, set_finished = Lwt.wait () in
      Hashtbl.add commit_locks key finished;
      Lwt.finalize
        (fun () ->
           let th, fn_cancel = fn ~priority ~switch in
           cancel := fn_cancel;
           th
        )
        (fun () ->
           Hashtbl.remove commit_locks key;
           Lwt.wakeup set_finished ();
           Lwt.return_unit
        )
  in
  aux (), (fun () -> !cancel ())

type connection = {
  sr : [`Submission_f4e8a768b32a7c42] Sturdy_ref.t;
  mutable sched : Cluster_api.Submission.t Lwt.t;
}

type t = {
  connection : connection;
  timeout : Duration.t option;
}

let tail ~job build_job =
  let rec aux start =
    Cluster_api.Job.log build_job start >>= function
    | Error (`Capnp e) -> Lwt.return @@ Fmt.error_msg "%a" Capnp_rpc.Error.pp e
    | Ok ("", _) -> Lwt_result.return ()
    | Ok (data, next) ->
      Current.Job.write job data;
      aux next
  in aux 0L

module Op = struct
  type nonrec t = t

  let id = "ci-ocluster-build"

  module Key = struct
    type t = {
      pool : string;                            (* The build pool to use (e.g. "linux-arm64") *)
      commit : Current_git.Commit_id.t;         (* The source code to build and test *)
      repo : Current_github.Repo_id.t;          (* Used to choose a build cache *)
      label : string;                           (* A unique ID for this build within the commit *)
    }

    let to_json { pool; commit; label; repo } =
      `Assoc [
        "pool", `String pool;
        "commit", `String (Current_git.Commit_id.hash commit);
        "repo", `String (Fmt.to_to_string Current_github.Repo_id.pp repo);
        "label", `String label;
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = struct
    type t = {
      ty : Spec.ty;
      base : Image.t;                           (* The image with the OCaml compiler to use. *)
      variant : string;                         (* Added as a comment in the Dockerfile *)
    }

    let to_json { base; ty; variant } =
      `Assoc [
        "base", `String (Image.digest base);
        "op", Spec.ty_to_yojson ty;
        "variant", `String variant;
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Outcome = Current.Unit

  (* Return a proxy to the scheduler, starting a new connection if we don't
     currently have a working one. *)
  let sched ~job t =
    let conn = t.connection in
    match Lwt.state conn.sched with
    | Lwt.Return cap when Capability.problem cap = None -> Lwt.return cap
    | Lwt.Sleep ->
      Current.Job.log job "Connecting to build cluster...";
      conn.sched      (* Already connecting; join that effort *)
    | _ ->
      Current.Job.log job "Connecting to build cluster...";
      let rec aux () =
        Lwt.catch
          (fun () ->
             Sturdy_ref.connect_exn conn.sr >>= fun cap ->
             Capability.wait_until_settled cap >|= fun () ->
             cap
          )
          (fun ex ->
             Log.warn (fun f -> f "Error connecting to build cluster (will retry): %a" Fmt.exn ex);
             Lwt_unix.sleep 10.0 >>= fun () ->
             aux ()
          )
      in
      conn.sched <- aux ();
      conn.sched

  (* Limit how many items we queue up at the scheduler (including assigned to workers). *)
  let rate_limit = Lwt_pool.create 200 Lwt.return

  (* This is called by [Current.Job] once the confirmation threshold allows the job to be submitted. *)
  let submit ~job ~pool ~action ~cache_hint ?src t ~priority ~switch:_ =
    let ticket_ref = ref None in
    let cancel () =
      match !ticket_ref with
      | None -> Lwt.return_unit
      | Some ticket ->
        Cluster_api.Ticket.cancel ticket >|= function
        | Ok () -> ()
        | Error (`Capnp e) -> Current.Job.log job "Cancel ticket failed: %a" Capnp_rpc.Error.pp e
    in
    let rec aux () =
      Prometheus.Gauge.inc_one Metrics.queue_connect;
      sched ~job t >>= fun sched ->
      Prometheus.Gauge.dec_one Metrics.queue_connect;
      let urgent = (priority = `High) in
      Prometheus.Gauge.inc_one Metrics.queue_rate_limit;
      Lwt_pool.use rate_limit (fun () ->
          Prometheus.Gauge.dec_one Metrics.queue_rate_limit;
          let ticket = Cluster_api.Submission.submit ~urgent ?src sched ~pool ~action ~cache_hint in
          let build_job = Cluster_api.Ticket.job ticket in
          ticket_ref := Some ticket;        (* Allow the user to cancel it now. *)
          Prometheus.Gauge.inc_one Metrics.queue_get_ticket;
          Capability.wait_until_settled ticket >>= fun () ->
          Prometheus.Gauge.dec_one Metrics.queue_get_ticket;
          Current.Job.log job "Waiting for worker...";
          Prometheus.Gauge.inc_one Metrics.queue_get_worker;
          Capability.wait_until_settled build_job >>= fun () ->
          Prometheus.Gauge.dec_one Metrics.queue_get_worker;
          ticket_ref := None;
          Capability.dec_ref ticket;
          Lwt.return build_job
        ) >>= fun build_job ->
      Lwt.pause () >>= fun () ->
      match Capability.problem build_job with
      | None -> Lwt.return build_job
      | Some err ->
        if Capability.problem sched = None then (
          (* The job failed but we're still connected to the scheduler. Report the error. *)
          Lwt.fail_with (Fmt.strf "%a" Capnp_rpc.Exception.pp err)
        ) else (
          aux ()
        )
    in
    aux (), cancel

  let hash_packages packages =
    Digest.string (String.concat "," packages) |> Digest.to_hex

  let get_cache_hint { Current_github.Repo_id.owner; name } { Value.base; variant; ty } =
    let deps =
      match ty with
      | `Opam (`Build, selection, _) -> hash_packages selection.packages
      | `Opam (`Lint `Doc, selection, _) -> hash_packages selection.packages
      | `Opam_fmt _ -> "ocamlformat"
      | `Duniverse -> "duniverse"
    in
    Printf.sprintf "%s/%s-%s-%s-%s"
      owner name
      (Image.hash base)
      variant deps

  let run t job { Key.pool; commit; label = _; repo } spec =
    let { Value.base; variant; ty } = spec in
    let make_dockerfile =
      let base = Image.hash base in
      match ty with
      | `Opam (`Build, selection, opam_files) -> Opam_build.dockerfile ~base ~opam_files ~selection
      | `Opam (`Lint `Doc, selection, opam_files) -> Lint.doc_dockerfile ~base ~opam_files ~selection
      | `Opam_fmt ocamlformat_source -> Lint.fmt_dockerfile ~base ~ocamlformat_source
      | `Duniverse -> Duniverse_build.dockerfile ~base ~repo ~variant
    in
    Current.Job.write job
      (Fmt.strf "@[<v>Base: %a@,%a@]@."
         Image.pp base
         Spec.pp_summary ty);
    Current.Job.write job
      (Fmt.strf "@.\
                 To reproduce locally:@.@.\
                 %a@.\
                 cat > Dockerfile <<'END-OF-DOCKERFILE'@.\
                 \o033[34m%a\o033[0m@.\
                 END-OF-DOCKERFILE@.\
                 docker build .@.@."
         Current_git.Commit_id.pp_user_clone commit
         Dockerfile.pp (make_dockerfile ~for_user:true));
    let dockerfile = Dockerfile.string_of_t (make_dockerfile ~for_user:false) in
    let options = { Cluster_api.Docker.Spec.defaults with buildkit = true } in
    let action = Cluster_api.Submission.docker_build ~options (`Contents dockerfile) in
    let src = (Git.Commit_id.repo commit, [Git.Commit_id.hash commit]) in
    let cache_hint = get_cache_hint repo spec in
    Current.Job.log job "Using cache hint %S" cache_hint;
    let build_pool = Current.Pool.of_fn ~label:"OCluster"
      @@ with_commit_lock ~job commit variant
      @@ submit ~job ~pool ~action ~cache_hint ~src t in
    Current.Job.start_with ~pool:build_pool job ?timeout:t.timeout ~level:Current.Level.Average >>= fun build_job ->
    Current.Job.write job (Fmt.strf "Using BuildKit Dockerfile:@.%s@." dockerfile);
    Capability.with_ref build_job @@ fun build_job ->
    let on_cancel _ =
      Cluster_api.Job.cancel build_job >|= function
      | Ok () -> ()
      | Error (`Capnp e) -> Current.Job.log job "Cancel failed: %a" Capnp_rpc.Error.pp e
    in
    Current.Job.with_handler job ~on_cancel @@ fun () ->
    let result = Cluster_api.Job.result build_job in
    tail ~job build_job >>!= fun () ->
    result >>= function
    | Error (`Capnp e) -> Lwt_result.fail (`Msg (Fmt.to_to_string Capnp_rpc.Error.pp e))
    | Ok _ ->
      Lwt_result.return ()

  let pp f ({ Key.pool; repo; commit; label }, _) =
    Fmt.pf f "test %a %a (%s:%s)"
      Current_github.Repo_id.pp repo
      Current_git.Commit_id.pp commit
      pool label

  let auto_cancel = true
  let latched = true
end

module BC = Current_cache.Generic(Op)

let config ?timeout sr =
  let connection = { sr; sched = Lwt.fail_with "init" } in
  { connection; timeout }

let build t ~platforms ~spec ~repo commit =
  Current.component "cluster build" |>
  let> { Spec.variant; ty; label } = spec
  and> commit = commit
  and> platforms = platforms
  and> repo = repo in
  match List.find_opt (fun p -> p.Platform.variant = variant) platforms with
  | Some { Platform.builder = _; pool; variant; base; _ } ->
    BC.run t { Op.Key.pool; commit; repo; label } { Op.Value.base; ty; variant }
  | None ->
    (* We can only get here if there is a bug. If the set of platforms changes, [Analyse] should recalculate. *)
    let msg = Fmt.strf "BUG: variant %S is not a supported platform" variant in
    Current_incr.const (Error (`Msg msg), None)

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with
  | Some { Current.Metadata.job_id; _ } -> job_id
  | None -> None

let v t ~platforms ~repo ~spec source =
  let build = build t ~platforms ~spec ~repo source in
  let+ state = Current.state ~hidden:true build
  and+ job_id = get_job_id build
  and+ spec = spec in
  let result =
    state |> Result.map @@ fun () ->
    match spec.ty with
    | `Duniverse
    | `Opam (`Build, _, _) -> `Built
    | `Opam (`Lint `Doc, _, _) -> `Checked
    | `Opam_fmt _ -> `Checked
  in
  result, job_id
