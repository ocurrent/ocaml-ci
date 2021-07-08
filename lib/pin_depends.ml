open Lwt.Infix
module Repo_map = Map.Make (String)

let ( >>!= ) x f = x >>= function Ok x -> f x | Error (`Msg m) -> failwith m

let repo_locks = ref Repo_map.empty

let repo_lock repo =
  match Repo_map.find_opt repo !repo_locks with
  | Some l -> l
  | None ->
      let l = Lwt_mutex.create () in
      repo_locks := Repo_map.add repo l !repo_locks;
      l

module Cmd = struct
  let id_of_repo repo =
    let base = Filename.basename repo in
    let digest = Digest.string repo |> Digest.to_hex in
    Fmt.strf "%s-%s" base digest

  (* .../var/pin-depends/myrepo-hhh *)
  let local_copy repo =
    let repos_dir = Current.state_dir "pin-depends" in
    Fpath.append repos_dir (Fpath.v (id_of_repo repo))

  let dir_exists path =
    match Unix.lstat (Fpath.to_string path) with
    | { Unix.st_kind = S_DIR; _ } -> true
    | _ -> false
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false

  let git ~cancellable ~job ?cwd args =
    let args =
      match cwd with None -> args | Some cwd -> "-C" :: Fpath.to_string cwd :: args
    in
    let cmd = Array.of_list ("git" :: args) in
    Current.Process.exec ~cancellable ~job ("", cmd)

  let git_clone ~cancellable ~job ~src dst =
    git ~cancellable ~job ["clone"; "-q"; src; Fpath.to_string dst]

  let git_fetch ~cancellable ~job repo = git ~cancellable ~job ~cwd:repo ["fetch"]

  let git_show ~job ~repo hash file =
    let cmd =
      ["git"; "-C"; Fpath.to_string repo; "show"; Printf.sprintf "%s:%s" hash file]
    in
    Current.Process.check_output ~cancellable:true ~job ("", Array.of_list cmd)
end

let clone ~job repo =
  Lwt_mutex.with_lock (repo_lock repo) @@ fun () ->
  let local_repo = Cmd.local_copy repo in
  (if Cmd.dir_exists local_repo then Cmd.git_fetch ~cancellable:true ~job local_repo
  else Cmd.git_clone ~cancellable:true ~job ~src:repo local_repo)
  >>!= fun () -> Lwt_result.return local_repo

let re_hash = Str.regexp "^[0-9A-Fa-f]+$"

let read_opam_file ~job ~repo ~hash pkg =
  let opam_filename = OpamPackage.name_to_string pkg ^ ".opam" in
  Cmd.git_show ~job ~repo hash opam_filename >>= function
  | Ok contents -> Lwt_result.return contents
  | Error (`Msg msg) -> (
      Cmd.git_show ~job ~repo hash ("opam/" ^ opam_filename) >>= function
      | Ok contents -> Lwt_result.return contents
      | Error _ ->
          Lwt.return
          @@ Fmt.error_msg "Can't find %s (or opam/%s): %s" opam_filename opam_filename
               msg)

let get_opam ~job ~pkg url =
  let { OpamUrl.transport; path = _; hash; backend } = url in
  if backend <> `git then
    Fmt.failwith "Only Git pin-depends are supported (got %S for %s)"
      (OpamUrl.to_string url) (OpamPackage.to_string pkg);
  if transport <> "https" then
    Fmt.failwith "Only 'git+https://' scheme is supported (got %S for %s)" transport
      (OpamPackage.to_string pkg);
  match hash with
  | None ->
      Fmt.failwith "Missing '#commit' in %S for package %s" (OpamUrl.to_string url)
        (OpamPackage.to_string pkg)
  | Some hash -> (
      if not (Str.string_match re_hash hash 0) then
        Fmt.failwith "Invalid commit hash %S for package %s" hash
          (OpamPackage.to_string pkg);
      let repo_url = OpamUrl.base_url url in
      let repo = Cmd.local_copy repo_url in
      read_opam_file ~job ~repo ~hash pkg >>= function
      | Ok contents -> Lwt.return contents
      | Error _ ->
          clone ~job repo_url >>!= fun repo ->
          read_opam_file ~job ~repo ~hash pkg >>!= fun contents -> Lwt.return contents)
