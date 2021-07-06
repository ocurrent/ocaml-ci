open Lwt.Infix

module Log = Ocaml_ci_api.Solver.Log
module Store = Git_unix.Store

let default_repo_url = "https://github.com/ocaml/opam-repository.git"

let sanitize_re = Str.regexp "[^A-Za-z-]"
let git_ext_re = Str.regexp "\\.git$"

let rec mkdir_p path =
  try Unix.mkdir path 0o700 with
  | Unix.Unix_error (EEXIST, _, _) -> ()
  | Unix.Unix_error (ENOENT, _, _) ->
      let parent = Filename.dirname path in
      mkdir_p parent;
      Unix.mkdir path 0o700

let repo_url_to_clone_path repo_url =
  (* The unit tests pass "opam-repository" as repo_url to refer to a local clone *)
  if repo_url = "opam-repository"
  then
    Fpath.v "opam-repository"
  else
    let uri = Uri.of_string repo_url in
    let sane_host = match Uri.host uri with
                    | Some host -> Str.global_replace sanitize_re "_" host
                    | None -> "no_host"
    in
    let sane_path = Uri.(path uri |> pct_decode |> Str.global_replace git_ext_re "" |> Str.global_replace sanitize_re "_") in
    Fpath.(v sane_host / sane_path)

let clone ?(repo_url = default_repo_url) () =
  let clone_path = repo_url_to_clone_path repo_url in
  let clone_parent = Fpath.parent clone_path |> Fpath.to_string in
  let clone_path_str = Fpath.to_string clone_path in
  begin match Unix.lstat clone_path_str with
    | Unix.{ st_kind = S_DIR; _ } -> Lwt.return_unit
    | _ -> Fmt.failwith "%S is not a directory!" clone_path_str
    | exception Unix.Unix_error(Unix.ENOENT, _, _) ->
      mkdir_p clone_parent;
      Process.exec ("", [| "git"; "clone"; "--bare"; repo_url; clone_path_str |])
  end

let open_store ?(repo_url = default_repo_url) () =
  clone ~repo_url () >>= fun () ->
  let path = repo_url_to_clone_path repo_url in
  Git_unix.Store.v ~dotgit:path path >|= function
  | Ok x -> x
  | Error e -> Fmt.failwith "Failed to open %a: %a" Fpath.pp path Store.pp_error e

let is_path_in_repo repo_url path =
  let clone_path = repo_url_to_clone_path repo_url in
  match Unix.lstat Fpath.(clone_path / path |> to_string) with
  | _ -> true
  | exception _ -> false

let partition_paths_by_repo commits paths =
  commits |> List.map (fun (repo_url, hash) ->
    ((repo_url, hash), paths |> List.filter (is_path_in_repo repo_url)))

let oldest_commit_with ~repo_url ~from paths =
  let clone_path = repo_url_to_clone_path repo_url |> Fpath.to_string in
  let cmd = "git" :: "-C" :: clone_path :: "log" :: "-n" :: "1" :: "--format=format:%H" :: from :: "--" :: paths in
  let cmd = ("", Array.of_list cmd) in
  Process.pread cmd >|= String.trim

let oldest_commits_with ~from pkgs =
  let paths =
    pkgs |> List.map (fun pkg ->
        let name = OpamPackage.name_to_string pkg in
        let version = OpamPackage.version_to_string pkg in
        Printf.sprintf "packages/%s/%s.%s" name name version
      )
  in
  partition_paths_by_repo from paths |> Lwt_list.map_p (fun ((repo_url, hash), paths) ->
    Lwt.bind
      (oldest_commit_with ~repo_url ~from:hash paths)
      (fun commit -> Lwt.return (repo_url, commit))
  )

let fetch ?(repo_url = default_repo_url) () =
  let clone_path = repo_url_to_clone_path repo_url |> Fpath.to_string in
  Process.exec ("", [| "git"; "-C"; clone_path; "fetch"; "origin"|])
