open Current.Syntax
open Lwt.Infix

module Raw = Current_docker.Raw

module Op = struct
  type t = Builder.t

  let id = "ci-build"

  module Key = struct
    type t = {
      commit : Current_git.Commit.t;            (* The source code to build and test *)
      repo : Current_github.Repo_id.t;          (* Used to choose a build cache *)
      base : Raw.Image.t;                       (* The image with the OCaml compiler to use. *)
      variant : string;                         (* Added as a comment in the Dockerfile *)
      analysis : Analyse.Analysis.t;
    }

    let digest_analysis x =
      let s = Analyse.Analysis.to_yojson x |> Yojson.Safe.to_string in
      `String (Digest.string s |> Digest.to_hex)

    let to_json { commit; analysis; base; variant; repo } =
      `Assoc [
        "commit", `String (Current_git.Commit.hash commit);
        "analysis", digest_analysis analysis;
        "base", `String (Raw.Image.digest base);
        "variant", `String variant;
        "repo", `String (Fmt.to_to_string Current_github.Repo_id.pp repo);
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = Current.Unit

  let or_raise = function
    | Ok () -> ()
    | Error (`Msg m) -> raise (Failure m)

  let build { Builder.docker_context; pool; build_timeout } job { Key.commit; base; analysis; variant; repo } =
    let make_dockerfile =
      let base = Raw.Image.hash base in
      if Analyse.Analysis.is_duniverse analysis then
        Duniverse_build.dockerfile ~base ~repo ~variant
      else
        Opam_build.dockerfile ~base ~info:analysis ~variant
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
    let cmd = Raw.Cmd.docker ~docker_context @@ ["build"; "--"; Fpath.to_string dir] in
    let pp_error_command f = Fmt.string f "Docker build" in
    Current.Process.exec ~cancellable:true ~pp_error_command ~job cmd

  let pp f { Key.repo; commit; variant; _ } =
    Fmt.pf f "@[<v2>test %a %a on %s@]"
      Current_github.Repo_id.pp repo
      Current_git.Commit.pp commit
      variant

  let auto_cancel = true
end

module BC = Current_cache.Make(Op)

let pull ~schedule platform =
  Current.component "docker pull" |>
  let> { Platform.builder; variant; label = _ } = platform in
  Builder.pull builder ("ocurrent/opam:" ^ variant) ~schedule

let build ~repo ~analysis ~platform ~base commit =
  Current.component "build" |>
  let> { Platform.builder; variant; _ } = platform
  and> analysis = analysis
  and> base = base
  and> commit = commit
  and> repo = repo in
  BC.get builder { Op.Key.commit; analysis; repo; base; variant }

let v ~platform ~schedule ~repo ~analysis source =
  let base = pull ~schedule platform in
  let+ () = build ~analysis ~repo ~platform ~base source in
  `Built
