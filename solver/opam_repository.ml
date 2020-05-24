open Lwt.Infix

module Log = Ocaml_ci_api.Solver.Log
module Store = Git_unix.Store

let clone_path = "opam-repository"

let open_store () =
  let path = Fpath.v clone_path in
  Git_unix.Store.v ~dotgit:path path >|= function
  | Ok x -> x
  | Error e -> Fmt.failwith "Failed to open opam-repository: %a" Store.pp_error e

let clone () =
  begin match Unix.lstat clone_path with
    | Unix.{ st_kind = S_DIR; _ } -> Lwt.return_unit
    | _ -> Fmt.failwith "%S is not a directory!" clone_path
    | exception Unix.Unix_error(Unix.ENOENT, _, "opam-repository") ->
      Process.exec ("", [| "git"; "clone"; "--bare"; "https://github.com/ocaml/opam-repository.git"; clone_path |])
  end

let oldest_commit_with ~from pkgs =
  let from = Store.Hash.to_hex from in
  let paths =
    pkgs |> List.map (fun pkg ->
        let name = OpamPackage.name_to_string pkg in
        let version = OpamPackage.version_to_string pkg in
        Printf.sprintf "packages/%s/%s.%s" name name version
      )
  in
  let cmd = "git" :: "-C" :: clone_path :: "log" :: "-n" :: "1" :: "--format=format:%H" :: from :: "--" :: paths in
  let cmd = ("", Array.of_list cmd) in
  Process.pread cmd >|= String.trim

let fetch () =
  Process.exec ("", [| "git"; "-C"; clone_path; "fetch"; "origin"|])
