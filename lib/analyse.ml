open Lwt.Infix
open Current.Syntax
module Worker = Ocaml_ci_api.Worker

let pool = Current.Pool.create ~label:"analyse" 20

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

(* A logging service that logs to [job]. *)
let job_log job =
  let module X = Ocaml_ci_api.Raw.Solve.Service.Log in
  X.local
  @@ object
       inherit X.service

       method write_impl params release_param_caps =
         let open X.Write in
         release_param_caps ();
         let msg = Params.msg_get params in
         Current.Job.write job msg;
         Capnp_rpc_lwt.Service.(return (Response.create_empty ()))
     end

module Analysis = struct
  type t = {
    opam_files : string list;
    ocamlformat_selection : Selection.t option;
    ocamlformat_source : Analyse_ocamlformat.source option;
    selections :
      [ `Opam_build of Selection.t list
      | `Opam_monorepo of Opam_monorepo.config list ];
  }
  [@@deriving yojson]

  let marshal t = to_yojson t |> Yojson.Safe.to_string

  let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with
    | Ok x -> x
    | Error e -> failwith e

  let opam_files t = t.opam_files
  let ocamlformat_selection t = t.ocamlformat_selection
  let ocamlformat_source t = t.ocamlformat_source
  let selections t = t.selections
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

  let opam_selections ~solve ~job ~platforms ~opam_files dir =
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
        let pinned_pkgs = pin_depends @ pinned_pkgs in
        Lwt_result.map
          (fun selections -> `Opam_build selections)
          (solve ~root_pkgs ~pinned_pkgs ~platforms))
      (function Failure msg -> Lwt_result.fail (`Msg msg) | ex -> Lwt.fail ex)

  let type_of_dir dir =
    match Opam_monorepo.detect ~dir with
    | Some info -> `Opam_monorepo info
    | None -> `Ocaml_repo

  (** Call the solver with a request containing these packages. When it returns
      a list, it is nonempty. *)
  let solve ~root_pkgs ~pinned_pkgs ~platforms ~opam_repository_commit ~job
      ~solver =
    let platforms =
      List.map
        (fun (variant, vars) -> (Variant.to_string variant, vars))
        platforms
    in
    let request =
      {
        Ocaml_ci_api.Worker.Solve_request.opam_repository_commits =
          [
            ( Current_git.Commit_id.repo opam_repository_commit,
              Current_git.Commit_id.hash opam_repository_commit );
          ];
        root_pkgs;
        pinned_pkgs;
        platforms;
      }
    in
    Current.Job.log job "Solving with opam-repository commit: %a"
      Current_git.Commit_id.pp opam_repository_commit;
    Capnp_rpc_lwt.Capability.with_ref (job_log job) @@ fun log ->
    Backend_solver.solve solver job request ~log >|= function
    | Error `Cancelled -> Fmt.error_msg "Job cancelled"
    | Error (`Msg msg) -> Fmt.error_msg "Error from solver: %s" msg
    | Ok [] -> Fmt.error_msg "No solution found for any supported platform"
    | Ok x -> (
        let has_no_platform (pkg_version, _) =
          if
            List.exists
              (fun (sel : Worker.Selection.t) ->
                List.mem pkg_version sel.compat_pkgs)
              x
          then None
          else Some pkg_version
        in
        match List.filter_map has_no_platform root_pkgs with
        | [] ->
            let root_pkgs = List.map fst root_pkgs in
            Ok (List.map (Selection.of_worker ~root_pkgs) x)
        | missing_pkgs ->
            Fmt.error_msg "No solution found for %a on any supported platform"
              Fmt.(list ~sep:comma Dump.string)
              missing_pkgs)

  let rec lwt_result_list_mapm ~f = function
    | [] -> Lwt_result.return []
    | x :: xs ->
        let open Lwt_result.Syntax in
        let+ y = f x and+ ys = lwt_result_list_mapm ~f xs in
        y :: ys

  let opam_dep_file packages =
    {|opam-version: "2.0"|}
    :: {|depends: [|}
    :: List.map
         (fun (pkg, ver) -> Printf.sprintf {|  "%s" %s|} pkg ver)
         packages
    @ [ "]\n" ]
    |> String.concat "\n"

  let exactly v = Printf.sprintf {|{ = "%s" }|} v

  (** Choose the platform that linting steps will be run on. Currently the Linux
      x86 platform with the lowest OCaml version *)
  let lint_selection selections =
    let selections =
      List.sort
        (fun x y ->
          Ocaml_version.compare
            (Variant.ocaml_version x.Selection.variant)
            (Variant.ocaml_version y.Selection.variant))
        selections
    in
    (* If there is for some reason no Linux x86 platform,
       fallback to whatever else there is *)
    List.find_opt
      (fun x ->
        Variant.arch x.Selection.variant == `X86_64
        && Variant.os x.Selection.variant == `linux)
      selections
    |> Option.value ~default:(List.hd selections)

  let solver_cache = Hashtbl.create 128

  let find_opam_repo_commit_for_ocamlformat ~solve ~platforms version =
    let ( let+ ) = Lwt_result.Infix.( >|= ) in
    let deps_for_ocamlformat =
      ( "deps_for_ocamlformat.opam",
        opam_dep_file [ ("ocamlformat", exactly version) ] )
    in
    let+ selections =
      match Hashtbl.find_opt solver_cache version with
      | Some (selections, platforms') ->
          if platforms <> platforms' then (
            let+ selections =
              solve ~root_pkgs:[ deps_for_ocamlformat ] ~pinned_pkgs:[]
                ~platforms
            in
            Hashtbl.add solver_cache version (selections, platforms);
            selections)
          else Lwt_result.return selections
      | None ->
          let+ selections =
            solve ~root_pkgs:[ deps_for_ocamlformat ] ~pinned_pkgs:[] ~platforms
          in
          Hashtbl.add solver_cache version (selections, platforms);
          selections
    in
    let selection = lint_selection selections in
    (selection.Selection.commit, selection)

  let of_dir ~solver ~job ~platforms ~opam_repository_commit root =
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
    let solve = solve ~opam_repository_commit ~job ~solver in
    let find_opam_repo_commit =
      find_opam_repo_commit_for_ocamlformat ~solve ~platforms
    in
    Analyse_ocamlformat.get_ocamlformat_source job ~opam_files ~root
      ~find_opam_repo_commit
    >>!= fun (ocamlformat_source, ocamlformat_selection) ->
    if opam_files = [] then Lwt_result.fail (`Msg "No opam files found!")
    else if List.filter Fpath.is_seg opam_files = [] then
      Lwt_result.fail (`Msg "No top-level opam files found!")
    else
      (match type_of_dir root with
      | `Opam_monorepo builds ->
          lwt_result_list_mapm builds ~f:(fun info ->
              Opam_monorepo.selection ~info ~solve ~platforms)
          |> Lwt_result.map (fun l -> `Opam_monorepo l)
      | `Ocaml_repo -> opam_selections ~solve ~job ~platforms ~opam_files root)
      >>!= fun selections ->
      let r =
        { opam_files; ocamlformat_selection; ocamlformat_source; selections }
      in
      Current.Job.log job "@[<v2>Results:@,%a@]"
        Yojson.Safe.(pretty_print ~std:true)
        (to_yojson r);
      Lwt_result.return r
end

module Examine = struct
  type t = Backend_solver.t

  module Key = struct
    type t = Current_git.Commit.t

    let digest src =
      let json = `Assoc [ ("src", `String (Current_git.Commit.hash src)) ] in
      Yojson.Safe.to_string json
  end

  module Value = struct
    type t = {
      opam_repository_commit : Current_git.Commit_id.t;
      platforms : (Variant.t * Worker.Vars.t) list;
    }

    let platform_to_yojson (variant, vars) =
      `Assoc
        [
          ("variant", Variant.to_yojson variant);
          ("vars", Worker.Vars.to_yojson vars);
        ]

    let digest { opam_repository_commit; platforms } =
      let json =
        `Assoc
          [
            ( "opam-repository",
              `String (Current_git.Commit_id.hash opam_repository_commit) );
            ("platforms", `List (List.map platform_to_yojson platforms));
          ]
      in
      Yojson.Safe.to_string json
  end

  module Outcome = Analysis

  let id = "ci-analyse"

  let run solver job src { Value.opam_repository_commit; platforms } =
    Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
    Current_git.with_checkout ~job ~pool src @@ fun src ->
    Analysis.of_dir ~solver ~platforms ~opam_repository_commit ~job src

  let pp f _ = Fmt.string f "Analyse"
  let auto_cancel = true
  let latched = true
end

module Examine_cache = Current_cache.Generic (Examine)

let examine ~solver ~platforms ~opam_repository_commit src =
  Current.component "Analyse"
  |> let> src and> opam_repository_commit and> platforms in
     let platforms =
       platforms
       |> List.map (fun { Platform.variant; vars; _ } -> (variant, vars))
     in
     Examine_cache.run solver src
       { Examine.Value.opam_repository_commit; platforms }
