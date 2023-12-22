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
    Fmt.str "%s-%s" base digest

  (* .../var/pin-depends/myrepo-hhh *)
  let local_copy repo =
    let repos_dir = Current.state_dir "pin-depends" in
    Fpath.append repos_dir (Fpath.v (id_of_repo repo))

  let dir_exists path =
    match Unix.lstat (Fpath.to_string path) with
    | { Unix.st_kind = S_DIR; _ } -> true
    | _ -> false
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false

  let file_exists path =
    match Unix.lstat (Fpath.to_string path) with
    | { Unix.st_kind = S_REG; _ } -> true
    | _ -> false
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false

  let git ~cancellable ~job ?cwd args =
    let args =
      match cwd with
      | None -> args
      | Some cwd -> "-C" :: Fpath.to_string cwd :: args
    in
    let cmd = Array.of_list ("git" :: args) in
    Current.Process.exec ~cancellable ~job ("", cmd)

  let git_clone ~cancellable ~job ~src dst =
    git ~cancellable ~job [ "clone"; "-q"; src; Fpath.to_string dst ]

  let git_fetch ~cancellable ~job repo =
    git ~cancellable ~job ~cwd:repo [ "fetch" ]

  let git_show ~job ~repo hash file =
    let cmd =
      [
        "git";
        "-C";
        Fpath.to_string repo;
        "show";
        Printf.sprintf "%s:%s" hash file;
      ]
    in
    Current.Process.check_output ~cancellable:true ~job ("", Array.of_list cmd)
end

let clone ~job repo =
  Lwt_mutex.with_lock (repo_lock repo) @@ fun () ->
  let local_repo = Cmd.local_copy repo in
  (if Cmd.dir_exists local_repo then
     Cmd.git_fetch ~cancellable:true ~job local_repo
   else Cmd.git_clone ~cancellable:true ~job ~src:repo local_repo)
  >>!= fun () -> Lwt_result.return local_repo

let re_hash = Str.regexp "^[0-9A-Fa-f]+$"

let first_ok_s f l =
  let rec apply x l =
    Lwt.apply f x >>= function
    | Ok v -> Lwt_result.return v
    | Error e -> loop e l
  and loop e = function [] -> Lwt_result.fail e | x :: l -> apply x l in
  match l with [] -> Lwt.fail Not_found | x :: l -> apply x l

let no_opam_file opam_filenames msg =
  let msg =
    Fmt.str "Can't find either of %a: %s"
      Fmt.(list ~sep:(any ", ") string)
      opam_filenames msg
  in
  `Msg msg

let read_opam_file ~job ~repo ~hash pkg =
  let opam_filenames =
    let opam_filename = OpamPackage.name_to_string pkg ^ ".opam" in
    [ opam_filename; "opam/" ^ opam_filename; "opam" ]
  in
  first_ok_s (Cmd.git_show ~job ~repo hash) opam_filenames
  |> Lwt_result.map_error @@ fun (`Msg msg) -> no_opam_file opam_filenames msg

let get_opam_git ~job ~pkg url =
  let { OpamUrl.transport; path = _; hash; backend = _ } = url in
  if transport <> "https" then
    Fmt.failwith "Only 'git+https://' scheme is supported (got %S for %s)"
      transport
      (OpamPackage.to_string pkg);
  match hash with
  | None ->
      Fmt.failwith "Missing '#commit' in %S for package %s"
        (OpamUrl.to_string url)
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
          read_opam_file ~job ~repo ~hash pkg >>!= fun contents ->
          Lwt.return contents)

module TgzLwtUnixWriter = struct
  type out_channel = Lwt_unix.file_descr
  type 'a t = 'a Lwt.t

  let really_write = Tar_lwt_unix.really_write
end

module TgzLwtUnixReader = struct
  type in_channel = Lwt_unix.file_descr
  type 'a t = 'a Lwt.t

  let really_read = Tar_lwt_unix.really_read

  let skip ic len =
    Lwt_unix.lseek ic len SEEK_CUR >>= fun skipped ->
    if len <> skipped then Fmt.failwith "Error reading pin-depend archive file.";
    Lwt.return_unit

  let read ic cs =
    let max = Cstruct.length cs in
    let buf = Bytes.create max in
    Lwt_unix.read ic buf 0 max >|= fun len ->
    Cstruct.blit_from_bytes buf 0 cs 0 len;
    len
end

module Tgz = Tar_gz.Make (Lwt) (TgzLwtUnixWriter) (TgzLwtUnixReader)

let memi e l =
  let rec aux i = function
    | [] -> raise Not_found
    | x :: l -> if String.compare x e = 0 then i else aux (i + 1) l
  in
  aux 0 l

let read_opam_file ~tarball ~prefix pkg =
  let opam_filenames =
    [
      prefix ^ "/" ^ OpamPackage.name_to_string pkg ^ ".opam";
      prefix ^ "/opam/" ^ OpamPackage.name_to_string pkg ^ ".opam";
      prefix ^ "/opam";
    ]
  in
  let read tgz hdr =
    let buf = Cstruct.create (hdr.Tar.Header.file_size |> Int64.to_int) in
    Tgz.really_read tgz buf >|= fun () -> Cstruct.to_string buf
  in
  let rec find acc tgz =
    Lwt.catch
      (fun () ->
        Tgz.get_next_header tgz >>= fun hdr ->
        (match (acc, memi hdr.Tar.Header.file_name opam_filenames) with
        | Some (priority, _), priority' when priority' <= priority ->
            read tgz hdr >>= fun contents ->
            Lwt.return_some (priority', contents)
        | None, priority' ->
            read tgz hdr >>= fun contents ->
            Lwt.return_some (priority', contents)
        | _ -> Lwt.return acc
        | exception Not_found -> Lwt.return acc)
        >>= fun acc ->
        let to_skip = Tar.Header.(Int64.to_int (to_sectors hdr) * length) in
        Tgz.skip tgz to_skip >>= fun () -> find acc tgz)
      (function
        | Tar.Header.End_of_stream -> Lwt.return acc | exn -> Lwt.fail exn)
  in
  Lwt_unix.openfile (Fpath.to_string tarball) [ O_RDONLY; O_CLOEXEC ] 0
  >>= fun fd ->
  Lwt.finalize
    (fun () ->
      let tgz = Tgz.of_in_channel ~internal:(Cstruct.create 4096) fd in
      find None tgz >>= function
      | Some (_, contents) -> Lwt_result.return contents
      | None -> Lwt_result.fail (no_opam_file opam_filenames "no such files."))
    (fun () -> Lwt_unix.close fd)

let fetch ~transport ~path ~tarball =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  Client.get (transport ^ "://" ^ path |> Uri.of_string) >>= fun (resp, _) ->
  if resp |> Response.status |> Code.code_of_status <> 302 then
    Fmt.failwith "Expected redirect response.";
  let location =
    resp |> Response.headers |> Fun.flip Header.get "Location" |> Option.get
  in
  Client.get (location |> Uri.of_string) >>= fun (resp, body) ->
  if resp |> Response.status |> Code.code_of_status <> 200 then
    Fmt.failwith "Failed to download the pin-depend: %a" Response.pp_hum resp;
  Lwt_io.with_file ~mode:Lwt_io.Output (Fpath.to_string tarball) @@ fun ch ->
  Lwt_stream.iter_s (Lwt_io.write ch) (Cohttp_lwt.Body.to_stream body)

let get_opam_http ~job ~pkg url =
  ignore job;
  (* TODO: support cancellation *)
  let { OpamUrl.transport; path; hash = _; backend = _ } = url in
  match Astring.String.cuts ~sep:"/" path with
  | [ "github.com"; _org; repo; "archive"; archive ] -> (
      let hash, ext =
        let hash, ext = Fpath.split_ext ~multi:true (Fpath.v archive) in
        (Fpath.to_string hash, ext)
      in
      if not (Str.string_match re_hash hash 0) then
        Fmt.failwith "Invalid commit hash %S for package %s" hash
          (OpamPackage.to_string pkg);
      if not (ext = ".tgz" || ext = ".tar.gz") then
        Fmt.failwith "Only gzipped tar archives are supported (for %s)" archive;
      let repo_url = OpamUrl.base_url url in
      let tarball = Cmd.local_copy repo_url in

      (if Cmd.file_exists tarball then Lwt.return_unit
       else
         Lwt.catch
           (fun () -> fetch ~transport ~path ~tarball)
           (fun exn ->
             Lwt_unix.unlink (Fpath.to_string tarball) >>= fun () ->
             Lwt.fail exn))
      >>= fun () ->
      read_opam_file ~tarball ~prefix:(repo ^ "-" ^ hash) pkg >>= function
      | Ok contents -> Lwt.return contents
      | Error (`Msg msg) ->
          Fmt.failwith "Couldn't get opam file %a contents from %s for %s: %s"
            Fpath.pp tarball (OpamUrl.to_string url)
            (OpamPackage.to_string pkg)
            msg)
  | _ ->
      Fmt.failwith
        "Only Git or GitHub https pin-depends are supported (got %S for %s)"
        (OpamUrl.to_string url)
        (OpamPackage.to_string pkg)

let get_opam ~job ~pkg url =
  match url.OpamUrl.backend with
  | `git -> get_opam_git ~job ~pkg url
  | `http -> get_opam_http ~job ~pkg url
  | _ ->
      Fmt.failwith
        "Only Git or GitHub https pin-depends are supported (got %S for %s)"
        (OpamUrl.to_string url)
        (OpamPackage.to_string pkg)
