open Current.Syntax
open Capnp_rpc_lwt
open Lwt.Infix

let src =
  Logs.Src.create "ocaml_ci.cluster_build" ~doc:"ocaml-ci ocluster builder"

module Log = (val Logs.src_log src : Logs.LOG)
module Image = Current_docker.Raw.Image
module Git = Current_git

let ( >>!= ) = Lwt_result.bind

type t = {
  connection : Current_ocluster.Connection.t;
  timeout : Duration.t option;
  on_cancel : string -> unit;
}

module Op = struct
  type nonrec t = t

  let id = "ci-ocluster-build"

  module Key = struct
    type t = {
      pool : Platform.Pool_name.t; (* The build pool to use (e.g. "linux-arm64") *)
      commit : Current_git.Commit_id.t; (* The source code to build and test *)
      repo : Repo_id.t; (* Used to choose a build cache *)
      label : string; (* A unique ID for this build within the commit *)
    }

    let to_json { pool; commit; label; repo } =
      `Assoc
        [
          ("pool", `String (Platform.Pool_name.to_string pool));
          ("commit", `String (Current_git.Commit_id.hash commit));
          ("repo", `String (Fmt.to_to_string Repo_id.pp repo));
          ("label", `String label);
        ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = struct
    type t = {
      ty : Spec.ty;
      base : Platform.base; (* The image with the OCaml compiler to use. *)
      variant : Variant.t; (* Added as a comment in the Dockerfile *)
    }

    let to_json { base; ty; variant } =
      `Assoc
        [
          ("base", Platform.to_yojson base);
          ("op", Spec.ty_to_yojson ty);
          ("variant", Variant.to_yojson variant);
        ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Outcome = Current.Unit

  let hash_packages packages =
    Digest.string (String.concat "," packages) |> Digest.to_hex

  let get_cache_hint { Repo_id.owner; name } { Value.base; variant; ty } =
    let deps =
      match ty with
      | `Opam (`Build, selection, _) -> hash_packages selection.packages
      | `Opam (`Lint (`Doc | `Opam), selection, _) ->
          hash_packages selection.packages
      | `Opam_fmt (selection, _) -> "ocamlformat-" ^ selection.Selection.commit
      | `Opam_monorepo _ -> "opam-monorepo-" ^ Variant.to_string variant
    in
    Fmt.str "%s/%s-%s-%a-%s" owner name (Platform.to_string base) Variant.pp
      variant deps

  let run t job { Key.pool; commit; label = _; repo } spec =
    Current.Job.on_cancel job (fun reason ->
        Logs.debug (fun l ->
            l "Calling the cluster on_cancel callback with reason: %s" reason);
        if reason <> "Job complete" then t.on_cancel reason;
        Lwt.return_unit)
    >>= fun () ->
    let { Value.base; variant; ty } = spec in
    let build_spec = Build.make_build_spec ~base ~repo ~variant ~ty in
    Current.Job.write job
      (Fmt.str "@[<v>Base: %a@,%a@]@." Platform.base_pp base Spec.pp_summary ty);
    Current.Job.write job
      (Fmt.str
         "@.To reproduce locally:@.@.%a@.cat > Dockerfile \
          <<'END-OF-DOCKERFILE'@.\o033[34m%s\o033[0m@.END-OF-DOCKERFILE@.docker \
          build .@.END-REPRO-BLOCK@.@."
         Current_git.Commit_id.pp_user_clone commit
         (Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false ~os:`Unix
            build_spec));
    let spec_str = Fmt.to_to_string Obuilder_spec.pp build_spec in
    let action = Cluster_api.Submission.obuilder_build spec_str in
    let src = (Git.Commit_id.repo commit, [ Git.Commit_id.hash commit ]) in
    let cache_hint = get_cache_hint repo spec in
    Current.Job.log job "Using cache hint %S" cache_hint;
    Current.Job.log job "Using OBuilder spec:@.%s@." spec_str;
    let build_pool =
      Current_ocluster.Connection.pool ~job ~pool:(Platform.Pool_name.to_string pool) ~action ~cache_hint ~src
        t.connection
    in
    Current.Job.start_with ~pool:build_pool job ?timeout:t.timeout
      ~level:Current.Level.Average
    >>= fun build_job ->
    Capability.with_ref build_job (Current_ocluster.Connection.run_job ~job)
    >>!= fun (_ : string) -> Lwt_result.return ()

  let pp f ({ Key.pool; repo; commit; label }, _) =
    Fmt.pf f "test %a %a (%s:%s)" Repo_id.pp repo Current_git.Commit_id.pp
      commit (Platform.Pool_name.to_string pool) label

  let auto_cancel = true
  let latched = true
end

module BC = Current_cache.Generic (Op)

let config ?timeout sr =
  let connection = Current_ocluster.Connection.create sr in
  { connection; timeout; on_cancel = ignore }

let build t ~platforms ~spec ~repo commit =
  Current.component "cluster build"
  |> let> { Spec.variant; ty; label } = spec
     and> commit
     and> platforms
     and> repo in
     match
       List.find_opt
         (fun p -> Variant.equal p.Platform.variant variant)
         platforms
     with
     | Some { Platform.builder = _; pool; variant; base; _ } ->
         BC.run t
           { Op.Key.pool; commit; repo; label }
           { Op.Value.base; ty; variant }
     | None ->
         (* We can only get here if there is a bug. If the set of platforms changes, [Analyse] should recalculate. *)
         let msg =
           Fmt.str "BUG: variant %a is not a supported platform" Variant.pp
             variant
         in
         Current_incr.const (Error (`Msg msg), None)

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with Some { Current.Metadata.job_id; _ } -> job_id | None -> None

let v t ?(on_cancel = ignore) ~platforms ~repo ~spec source =
  let t = { t with on_cancel } in
  let build = build t ~platforms ~spec ~repo source in
  let+ state = Current.state ~hidden:true build
  and+ job_id = get_job_id build
  and+ spec in
  let result =
    state
    |> Result.map @@ fun () ->
       match spec.ty with
       | `Opam_monorepo _ | `Opam (`Build, _, _) -> `Built
       | `Opam (`Lint (`Doc | `Opam), _, _) -> `Checked
       | `Opam_fmt _ -> `Checked
  in
  (result, job_id)
