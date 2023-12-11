open Lwt.Infix
open Current.Syntax
module Commit = Current_git.Commit
module Commit_id = Current_git.Commit_id

let pool = Current.Pool.create ~label:"extract_commit" 20

let is_empty_file x =
  match Unix.lstat (Fpath.to_string x) with
  | Unix.{ st_kind = S_REG; st_size = 0; _ } -> true
  | _ -> false

let ( >>!= ) = Lwt_result.bind

let read_file ~max_len path =
  Lwt_io.with_file ~mode:Lwt_io.input path (fun ch ->
      Lwt_io.length ch >>= fun len ->
      let len =
        if len <= Int64.of_int max_len then Int64.to_int len
        else Fmt.failwith "File %S too big (%Ld bytes)" path len
      in
      let buf = Bytes.create len in
      Lwt_io.read_into_exactly ch buf 0 len >|= fun () -> Bytes.to_string buf)

module Content = struct
  type ('a, 'b) result = ('a, 'b) Stdlib.result = Ok of 'a | Error of 'b
  [@@deriving yojson]

  type t = {
    opam_files : string list;
    root_pkgs : (string * string) list;
    pinned_pkgs : (string * string) list;
    ocamlformat_version : (string option, [ `Msg of string ]) result;
    dir_type : [ `Opam_monorepo of Opam_monorepo.info list | `Ocaml_repo ];
  }
  [@@deriving yojson]

  let marshal t = to_yojson t |> Yojson.Safe.to_string

  let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with
    | Ok x -> x
    | Error e -> failwith e

  let root_pkgs t = t.root_pkgs
  let pinned_pkgs t = t.pinned_pkgs
  let ocamlformat_version t = t.ocamlformat_version
  let opam_files t = t.opam_files
  let dir_type t = t.dir_type
  let is_test_dir = Astring.String.is_prefix ~affix:"test"

  let check_opam_version =
    let version_2 = OpamVersion.of_string "2" in
    fun name opam ->
      let opam_version = OpamFile.OPAM.opam_version opam in
      if OpamVersion.compare opam_version version_2 < 0 then
        Fmt.failwith "Package %S uses unsupported opam version %s (need >= 2)"
          name
          (OpamVersion.to_string opam_version)

  (* For each package in [root_pkgs], parse the opam file and check whether it uses pin-depends.
     Fetch and return all pinned opam files. Also, ensure we're using opam format version 2. *)
  let handle_opam_files ~job ~root_pkgs ~pinned_pkgs =
    pinned_pkgs
    |> List.iter (fun (name, contents) ->
           check_opam_version name (OpamFile.OPAM.read_from_string contents));
    let pin_depends =
      root_pkgs
      |> List.map (fun (name, contents) ->
             let opam =
               try OpamFile.OPAM.read_from_string contents
               with ex ->
                 Fmt.failwith "Invalid opam file %S: %a" name Fmt.exn ex
             in
             check_opam_version name opam;
             let pin_depends = OpamFile.OPAM.pin_depends opam in
             pin_depends
             |> List.map (fun (pkg, url) ->
                    Current.Job.log job "%s: found pin-depends: %s -> %s" name
                      (OpamPackage.to_string pkg)
                      (OpamUrl.to_string url);
                    (name, pkg, url)))
      |> List.concat
    in
    pin_depends
    |> Lwt_list.map_s (fun (root_pkg, pkg, url) ->
           Lwt.catch
             (fun () ->
               Pin_depends.get_opam ~job ~pkg url >|= fun contents ->
               (OpamPackage.to_string pkg, contents))
             (function
               | Failure msg ->
                   Fmt.failwith "%s (processing pin-depends in %s)" msg root_pkg
               | ex -> Lwt.fail ex))

  let get_all_pinned_pkgs job opam_files dir =
    let src = Fpath.to_string dir in
    let ( / ) = Filename.concat in
    opam_files
    |> Lwt_list.fold_left_s
         (fun (root_pkgs, pinned_pkgs) path ->
           let name = Filename.basename path |> Filename.chop_extension in
           let name =
             if String.contains name '.' then name else name ^ ".dev"
           in
           read_file ~max_len:102400 (src / path) >|= fun file ->
           let item = (name, file) in
           if Filename.dirname path = "." then (item :: root_pkgs, pinned_pkgs)
           else (root_pkgs, item :: pinned_pkgs))
         ([], [])
    >>= fun (root_pkgs, pinned_pkgs) ->
    Lwt.try_bind
      (fun () -> handle_opam_files ~job ~root_pkgs ~pinned_pkgs)
      (fun pin_depends ->
        Lwt_result.return (root_pkgs, pin_depends @ pinned_pkgs))
      (function Failure msg -> Lwt_result.fail (`Msg msg) | ex -> Lwt.fail ex)

  let get_ocamlformat_version job ~root =
    Analyse_ocamlformat.ocamlformat_version_from_file ~root job
      Fpath.(to_string (root / ".ocamlformat"))

  let type_of_dir dir :
      [ `Opam_monorepo of Opam_monorepo.info list | `Ocaml_repo ] =
    match Opam_monorepo.detect ~dir with
    | Some info -> `Opam_monorepo info
    | None -> `Ocaml_repo

  let of_dir ~job root =
    let dir_type = type_of_dir root in
    let cancelled = Atomic.make None in
    let fold_on_opam_files () =
      let module M = struct
        exception Exit of string
      end in
      let module S = Astring.String.Set in
      let opam_files full_path =
        let path = Option.get (Fpath.rem_prefix root full_path) in
        let consider_opam_file =
          match Fpath.segs path with
          | [] | [ _ ] -> true
          | segs ->
              if List.exists is_test_dir segs then (
                Current.Job.log job "Ignoring test directory %a" Fpath.pp path;
                false)
              else true
        in
        if is_empty_file full_path then (
          Current.Job.log job "WARNING: ignoring empty opam file %a" Fpath.pp
            path;
          None)
        else if consider_opam_file then Some path
        else None
      in
      let is_opam_ext path = Ok (Fpath.has_ext "opam" path) in
      let traverse path =
        match Fpath.rem_prefix root path with
        (* maxdepth=3 *)
        | Some suffix -> Ok (List.compare_length_with (Fpath.segs suffix) 3 <= 0)
        | None when Fpath.equal root path -> Ok true
        | None ->
            Fmt.error_msg "%a is not a prefix of %a" Fpath.pp root Fpath.pp path
      in
      let add_opam_files path acc =
        Option.iter (fun s -> raise_notrace (M.Exit s)) (Atomic.get cancelled);
        match opam_files path with
        | Some path -> S.add (Fpath.to_string path) acc
        | None -> acc
      in
      (try
         Bos.OS.Path.fold ~elements:(`Sat is_opam_ext) ~traverse:(`Sat traverse)
           add_opam_files S.empty [ root ]
       with M.Exit reason ->
         Fmt.error_msg "Cancelling opam file lookup (%s)" reason)
      |> Result.map S.elements
    in
    Current.Job.on_cancel job (fun reason ->
        Atomic.set cancelled (Some reason);
        Lwt.return_unit)
    >>= fun () ->
    Lwt_preemptive.detach fold_on_opam_files () >>!= fun opam_files ->
    get_ocamlformat_version job ~root >>= function
    | ocamlformat_version ->
        get_all_pinned_pkgs job opam_files root
        >>!= fun (root_pkgs, pinned_pkgs) ->
        Lwt_result.return
          { root_pkgs; pinned_pkgs; ocamlformat_version; opam_files; dir_type }
end

module Extract = struct
  let id = "ci-extract"

  type t = No_context

  module Key = struct
    type t = Commit.t

    let digest t = Commit.hash t
  end

  module Value = struct
    type t = Commit.t

    let digest t = Commit.hash t
  end

  module Outcome = Content

  let run _ job _ src =
    Current.Job.start job ~pool ~level:Current.Level.Harmless >>= fun () ->
    Current_git.with_checkout ~job src @@ fun src -> Content.of_dir ~job src

  let pp f _ = Fmt.pf f "Extract"
  let auto_cancel = true
  let latched = true
end

module Extract_cache = Current_cache.Generic (Extract)

let extract src =
  Current.component "Extract"
  |> let> src in
     Extract_cache.run No_context src src
