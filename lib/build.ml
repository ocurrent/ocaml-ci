open Current.Syntax
open Lwt.Infix

module Raw = Current_docker.Raw
module Selection = Ocaml_ci_api.Worker.Selection

let checkout_pool = Current.Pool.create ~label:"git-clone" 1

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
      variant : Variant.t;                      (* Added as a comment in the Dockerfile *)
    }

    let to_json { base; ty; variant } =
      `Assoc [
        "base", `String (Raw.Image.digest base);
        "op", Spec.ty_to_yojson ty;
        "variant", (Variant.to_yojson variant);
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Outcome = Current.Unit

  let or_raise = function
    | Ok () -> ()
    | Error (`Msg m) -> raise (Failure m)

  let run { Builder.docker_context; pool; build_timeout } job
      { Key.commit; label = _; repo } { Value.base; variant; ty } =
    let build_spec =
      let base = Raw.Image.hash base in
      match ty with
      | `Opam (`Build, selection, opam_files) -> Opam_build.spec ~base ~opam_files ~selection
      | `Opam (`Lint `Doc, selection, opam_files) -> Lint.doc_spec ~base ~opam_files ~selection
      | `Opam (`Lint `Opam, _selection, opam_files) -> Lint.opam_lint_spec ~base ~opam_files
      | `Opam_fmt ocamlformat_source -> Lint.fmt_spec ~base ~ocamlformat_source
      | `Duniverse opam_files -> Duniverse_build.spec ~base ~repo ~opam_files ~variant
      | `Opam_monorepo config -> Opam_monorepo.spec ~base ~repo ~config ~variant
    in
    let make_dockerfile ~for_user =
      let open Dockerfile in
      (if for_user then empty else Buildkit_syntax.add (Variant.arch variant)) @@
      Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:(not for_user) build_spec
    in
    Current.Job.write job
      (Fmt.strf "@[<v>Base: %a@,%a@]@."
         Raw.Image.pp base
         Spec.pp_summary ty);
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
    Fmt.pf f "test %a %a (%s)"
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
  match List.find_opt (fun p -> Variant.equal p.Platform.variant variant) platforms with
  | Some { Platform.builder; variant; base; _ } ->
    BC.run builder { Op.Key.commit; repo; label } { Op.Value.base; ty; variant }
  | None ->
    (* We can only get here if there is a bug. If the set of platforms changes, [Analyse] should recalculate. *)
    let msg = Fmt.strf "BUG: variant %a is not a supported platform" Variant.pp variant in
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
    | `Duniverse _
    | `Opam_monorepo _
    | `Opam (`Build, _, _) -> `Built
    | `Opam (`Lint (`Doc|`Opam), _, _) -> `Checked
    | `Opam_fmt _ -> `Checked
  in
  result, job_id
