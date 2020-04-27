open Current.Syntax
open Lwt.Infix

module Raw = Current_docker.Raw

let checkout_pool = Current.Pool.create ~label:"git-clone" 1

module Spec = struct
  type analysis = Analyse.Analysis.t

  let analysis_to_yojson x =
    let s = Analyse.Analysis.to_yojson x |> Yojson.Safe.to_string in
    `String (Digest.string s |> Digest.to_hex)

  type ty = [
    | `Opam of [ `Build | `Lint of [ `Fmt | `Doc ]] * analysis
    | `Duniverse
  ] [@@deriving to_yojson]

  type t = {
    label : string;
    variant : string;
    ty : ty;
  }

  let opam ~label ~variant ~analysis op =
    { label; variant; ty = `Opam (op, analysis) }

  let duniverse ~label ~variant =
    { label; variant; ty = `Duniverse }

  let pp f t = Fmt.string f t.label
  let compare a b = compare a.label b.label
  let label t = t.label
end

(* Make sure we never build the same (commit, variant) twice at the same time, as this is likely
   to trigger BuildKit bug https://github.com/moby/buildkit/issues/1456.
   While a build is in progress, this contains a promise for the build finishing. *)
let commit_locks = Hashtbl.create 1000

let rec with_commit_lock ~job commit variant fn =
  let key = (Current_git.Commit.hash commit, variant) in
  match Hashtbl.find_opt commit_locks key with
  | Some lock ->
    Current.Job.log job "Waiting for a similar build to finish...";
    lock >>= fun () ->
    with_commit_lock ~job commit variant fn
  | None ->
    let finished, set_finished = Lwt.wait () in
    Hashtbl.add commit_locks key finished;
    Lwt.finalize fn
      (fun () ->
         Hashtbl.remove commit_locks key;
         Lwt.wakeup set_finished ();
         Lwt.return_unit
      )

module Op = struct
  type t = Builder.t

  let id = "ci-build"

  let dockerignore = ".git"

  module Key = struct
    type t = {
      commit : Current_git.Commit.t;            (* The source code to build and test *)
      repo : Current_github.Repo_id.t;          (* Used to choose a build cache *)
      label : string;                           (* A unique ID for this build within the commit *)
    }

    let to_json { commit; label; repo } =
      `Assoc [
        "commit", `String (Current_git.Commit.hash commit);
        "repo", `String (Fmt.to_to_string Current_github.Repo_id.pp repo);
        "label", `String label;
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = struct
    type t = {
      ty : Spec.ty;
      base : Raw.Image.t;                       (* The image with the OCaml compiler to use. *)
      variant : string;                         (* Added as a comment in the Dockerfile *)
    }

    let to_json { base; ty; variant } =
      `Assoc [
        "base", `String (Raw.Image.digest base);
        "op", Spec.ty_to_yojson ty;
        "variant", `String variant;
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Outcome = Current.Unit

  let or_raise = function
    | Ok () -> ()
    | Error (`Msg m) -> raise (Failure m)

  let run { Builder.docker_context; pool; build_timeout } job
      { Key.commit; label = _; repo } { Value.base; variant; ty } =
    let make_dockerfile =
      let base = Raw.Image.hash base in
      match ty with
      | `Opam (`Build, analysis) ->
        Opam_build.dockerfile ~base ~info:analysis ~variant
      | `Opam (`Lint `Fmt, analysis) -> Lint.fmt_dockerfile ~base ~info:analysis ~variant
      | `Opam (`Lint `Doc, analysis) -> Lint.doc_dockerfile ~base ~info:analysis ~variant
      | `Duniverse ->
        Duniverse_build.dockerfile ~base ~repo ~variant
    in
    Current.Job.write job
      (Fmt.strf "@.\
                 To reproduce locally:@.@.\
                 %a@.\
                 cat > Dockerfile <<'END-OF-DOCKERFILE'@.\
                 \o033[34m%a\o033[0m@.\
                 END-OF-DOCKERFILE@.\
                 docker build .@.@."
         Current_git.Commit_id.pp_user_clone (Current_git.Commit.id commit)
         Dockerfile.pp (make_dockerfile ~for_user:true));
    let dockerfile = Dockerfile.string_of_t (make_dockerfile ~for_user:false) in
    Current.Job.start ~timeout:build_timeout ~pool job ~level:Current.Level.Average >>= fun () ->
    with_commit_lock ~job commit variant @@ fun () ->
    Current_git.with_checkout ~pool:checkout_pool ~job commit @@ fun dir ->
    Current.Job.write job (Fmt.strf "Writing BuildKit Dockerfile:@.%s@." dockerfile);
    Bos.OS.File.write Fpath.(dir / "Dockerfile") (dockerfile ^ "\n") |> or_raise;
    Bos.OS.File.write Fpath.(dir / ".dockerignore") dockerignore |> or_raise;
    let cmd = Raw.Cmd.docker ~docker_context @@ ["build"; "--"; Fpath.to_string dir] in
    let pp_error_command f = Fmt.string f "Docker build" in
    Current.Process.exec ~cancellable:true ~pp_error_command ~job cmd

  let pp f ({ Key.repo; commit; label }, _) =
    Fmt.pf f "@[<v2>test %a %a (%s)@]"
      Current_github.Repo_id.pp repo
      Current_git.Commit.pp commit
      label

  let auto_cancel = true
  let latched = true
end

module BC = Current_cache.Generic(Op)

let build ~platforms ~spec ~repo commit =
  Current.component "build" |>
  let> { Spec.variant; ty; label } = spec
  and> commit = commit
  and> platforms = platforms
  and> repo = repo in
  match List.find_opt (fun p -> p.Platform.variant = variant) platforms with
  | Some { Platform.builder; variant; base; _ } ->
    BC.run builder { Op.Key.commit; repo; label } { Op.Value.base; ty; variant }
  | None ->
    (* We can only get here if there is a bug. If the set of platforms changes, [Analyse] should recalculate. *)
    let msg = Fmt.strf "BUG: variant %S is not a supported platform" variant in
    Current_incr.const (Error (`Msg msg), None)

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with
  | Some { Current.Metadata.job_id; _ } -> job_id
  | None -> None

let v ~platforms ~repo ~spec source =
  let build = build ~platforms ~spec ~repo source in
  let+ state = Current.state ~hidden:true build
  and+ job_id = get_job_id build
  and+ spec = spec in
  let result =
    state |> Result.map @@ fun () ->
    match spec.ty with
    | `Duniverse
    | `Opam (`Build, _) -> `Built
    | `Opam (`Lint _, _) -> `Checked
  in
  result, job_id
