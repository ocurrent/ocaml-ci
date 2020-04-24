open Current.Syntax
open Lwt.Infix

module Raw = Current_docker.Raw

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
    platform : Platform.t;
    ty : ty;
  }

  let opam ~label ~platform ~analysis op =
    { label; platform; ty = `Opam (op, analysis) }

  let duniverse ~label ~platform =
    { label; platform; ty = `Duniverse }

  let pp f t = Fmt.string f t.label
  let compare a b = compare a.label b.label
  let label t = t.label
end

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
    Current_git.with_checkout ~job commit @@ fun dir ->
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

let pull ~schedule spec =
  Current.component "docker pull" |>
  let> { Spec.platform; _} = spec in
  let { Platform.builder; variant; label = _ } = platform in
  Builder.pull builder ("ocurrent/opam:" ^ variant) ~schedule

let build ~spec ~repo ~base commit =
  Current.component "build" |>
  let> { Spec.platform; ty; label } = spec
  and> base = base
  and> commit = commit
  and> repo = repo in
  let { Platform.builder; variant; _ } = platform in
  BC.run builder { Op.Key.commit; repo; label } { Op.Value.base; ty; variant }

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with
  | Some { Current.Metadata.job_id; _ } -> job_id
  | None -> None

let v ~schedule ~repo ~spec source =
  let base = pull ~schedule spec in
  let build = build ~spec ~repo ~base source in
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
