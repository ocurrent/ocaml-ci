open Lwt.Infix
open Current.Syntax

module Worker = Ocaml_ci_api.Worker

let pool = Current.Pool.create ~label:"analyse" 2

let is_empty_file x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_REG; st_size = 0; _ } -> true
  | _ -> false

let is_toplevel path = not (String.contains path '/')

let ( >>!= ) = Lwt_result.bind

let read_file ~max_len path =
  let ch = open_in path in
  Fun.protect ~finally:(fun () -> close_in ch)
    (fun () ->
       let len = in_channel_length ch in
       if len <= max_len then really_input_string ch len
       else Fmt.failwith "File %S too big (%d bytes)" path len
    )

module Analysis = struct
  type t = {
    opam_files : string list;
    ocamlformat_source : Analyse_ocamlformat.source option;
    selections : [ `Opam_build of Worker.Selection.t list | `Duniverse of string list ];
  }
  [@@deriving yojson]

  let marshal t = to_yojson t |> Yojson.Safe.to_string

  let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with
    | Ok x -> x
    | Error e -> failwith e

  let opam_files t = t.opam_files

  let is_duniverse t =
    match t.selections with
    | `Duniverse _ -> true
    | `Opam_build _ -> false

  let ocamlformat_source t = t.ocamlformat_source

  let selections t = t.selections

  let is_test_dir = Astring.String.is_prefix ~affix:"test"

  let get_ocaml_versions dir =
    (* TODO Just manual right now to avoid a build dep on Duniverse_lib *)
    let dune_get_path = Filename.concat (Fpath.to_string dir) "dune-get" in
    let open Sexplib.Sexp in
    match load_sexp dune_get_path with
    | sxp -> begin
        match sxp with
        | List (List (Atom "config" :: List cs :: _) :: _) -> begin
            List.filter_map (function
                | List (Atom "ocaml_compilers" :: List os :: _) ->
                  Some (List.filter_map (function Atom s -> Some s | _ -> None) os)
                | _ -> None) cs |> List.flatten
          end
        | _ -> []
      end
    | exception exn ->
      Fmt.failwith "Exception parsing dune-get: %a" Fmt.exn exn

  let pp_ocaml_version f (_variant, vars) =
    Fmt.string f vars.Worker.Vars.ocaml_version

  let ocaml_major_version vars =
    Ocaml_version.with_just_major_and_minor (Ocaml_version.of_string_exn vars.Worker.Vars.ocaml_version)

  let duniverse_selections ~job ~platforms dir =
    let vs = get_ocaml_versions dir in
    if vs = [] then failwith "No OCaml compilers specified!";
    let find_compiler v =
      let v = Ocaml_version.with_just_major_and_minor (Ocaml_version.of_string_exn v) in
      let matches_compiler (_variant, vars) =
        Ocaml_version.compare v (ocaml_major_version vars) = 0
      in
      match List.find_opt matches_compiler platforms with
      | None ->
        Current.Job.log job "WARNING: Unsupported compiler version %a (have: %a)"
          Ocaml_version.pp v Fmt.(Dump.list pp_ocaml_version) platforms;
        None
      | Some (variant, _vars) ->
        Some variant
    in
    let variants = List.filter_map find_compiler vs in
    if variants = [] then failwith "No supported compilers found!";
    Lwt_result.return (`Duniverse variants)

  let check_opam_version =
    let version_2 = OpamVersion.of_string "2" in
    fun name opam ->
      let opam_version = OpamFile.OPAM.opam_version opam in
      if OpamVersion.compare opam_version version_2 < 0 then
        Fmt.failwith "Package %S uses unsupported opam version %s (need >= 2)" name (OpamVersion.to_string opam_version)

  (* For each package in [root_pkgs], parse the opam file and check whether it uses pin-depends.
     Fetch and return all pinned opam files. Also, ensure we're using opam format version 2. *)
  let handle_opam_files ~job ~root_pkgs ~pinned_pkgs =
    pinned_pkgs |> List.iter (fun (name, contents) ->
        check_opam_version name (OpamFile.OPAM.read_from_string contents)
      );
    let pin_depends =
      root_pkgs
      |> List.map (fun (name, contents) ->
          try
            let opam = OpamFile.OPAM.read_from_string contents in
            check_opam_version name opam;
            let pin_depends = OpamFile.OPAM.pin_depends opam in
            pin_depends |> List.map (fun (pkg, url) ->
                Current.Job.log job "%s: found pin-depends: %s -> %s"
                  name (OpamPackage.to_string pkg) (OpamUrl.to_string url);
                (name, pkg, url)
              )
          with ex ->
            Fmt.failwith "Invalid opam file %S: %a" name Fmt.exn ex
        )
      |> List.concat
    in
    pin_depends |> Lwt_list.map_s (fun (root_pkg, pkg, url) ->
        Lwt.catch
          (fun () ->
             Pin_depends.get_opam ~job ~pkg url >|= fun contents ->
             (OpamPackage.to_string pkg, contents)
          )
          (function
            | Failure msg -> Fmt.failwith "%s (processing pin-depends in %s)" msg root_pkg
            | ex -> Lwt.fail ex
          )
      )

  let opam_selections ~solver ~job ~platforms ~opam_repository ~opam_files dir =
    let src = Fpath.to_string dir in
    let ( / ) = Filename.concat in
    let root_pkgs, pinned_pkgs =
      opam_files |> List.fold_left (fun (root_pkgs, pinned_pkgs) path ->
          let name = Filename.basename path |> Filename.chop_extension in
          let name = if String.contains name '.' then name else name ^ ".dev" in
          let item = name, read_file ~max_len:102400 (src / path) in
          if Filename.dirname path = "." then
            (item :: root_pkgs, pinned_pkgs)
          else
            (root_pkgs, item :: pinned_pkgs)
        ) ([], [])
    in
    Lwt_result.catch (handle_opam_files ~job ~root_pkgs ~pinned_pkgs) >>= function
    | Error (Failure msg) ->
      Lwt_result.fail (`Msg msg)
    | Error ex -> Lwt.fail ex
    | Ok pin_depends ->
      let pinned_pkgs = pin_depends @ pinned_pkgs in
      let request = { Ocaml_ci_api.Worker.Solve_request.
                      opam_repository = Fpath.to_string opam_repository;
                      root_pkgs;
                      pinned_pkgs;
                      platforms
                    } in
      let stdin = Yojson.Safe.to_string (Ocaml_ci_api.Worker.Solve_request.to_yojson request) in
      Current.Process.check_output ~job ~stdin ~cancellable:false solver >>!= fun response ->
      let json = Yojson.Safe.from_string response in
      match Worker.Solve_response.of_yojson json with
      | Error msg -> Lwt.return (Fmt.error_msg "Bad solver response: %s" msg)
      | Ok response ->
        match response with
        | Ok [] -> Lwt.return (Fmt.error_msg "No solution found for any supported platform")
        | Ok x -> Lwt_result.return (`Opam_build x)
        | Error (`Msg msg) -> Lwt.return (Fmt.error_msg "Error from solver: %s" msg)

  let of_dir ~solver ~job ~platforms ~opam_repository dir =
    let is_duniverse = Sys.file_exists (Filename.concat (Fpath.to_string dir) "dune-get") in
    let cmd = "", [| "find"; "."; "-maxdepth"; "3"; "-name"; "*.opam" |] in
    Current.Process.check_output ~cwd:dir ~cancellable:true ~job cmd >>!= fun output ->
    let opam_files =
      String.split_on_char '\n' output
      |> List.sort String.compare
      |> List.filter_map (function
          | "" -> None
          | path ->
            let path =
              if Astring.String.is_prefix ~affix:"./" path then
                Astring.String.with_range ~first:2 path
              else path
            in
            let check_whitelist_path path =
              match Fpath.v path |> Fpath.segs with
              | [_file] -> true
              | ["duniverse"; _pkg; _file] -> true
              | _ when is_duniverse ->
                Current.Job.log job "WARNING: ignoring opam file %S as not in root or duniverse subdir" path; false
              | segs when List.exists is_test_dir segs ->
                Current.Job.log job "Ignoring test directory %S" path;
                false
              | _ -> true
            in
            let full_path = Filename.concat (Fpath.to_string dir) path in
            if is_empty_file full_path then (
              Current.Job.log job "WARNING: ignoring empty opam file %S" path;
              None
            ) else if check_whitelist_path path then
              Some path
            else None
        )
    in
    (* [opam_files] are used to detect vendored OCamlformat but this only works
       with duniverse, as other opam files are filtered above. *)
    Analyse_ocamlformat.get_ocamlformat_source job ~opam_files ~root:dir >>= fun ocamlformat_source ->
    if opam_files = [] then Lwt_result.fail (`Msg "No opam files found!")
    else if List.filter is_toplevel opam_files = [] then Lwt_result.fail (`Msg "No top-level opam files found!")
    else (
      begin
        if is_duniverse then duniverse_selections ~job ~platforms dir
        else opam_selections ~solver ~job ~platforms ~opam_repository ~opam_files dir
      end >>!= fun selections ->
      let r = { opam_files; ocamlformat_source; selections } in
      Current.Job.log job "@[<v2>Results:@,%a@]" Yojson.Safe.(pretty_print ~std:true) (to_yojson r);
      Lwt_result.return r
    )
end

module Examine = struct
  type t = Lwt_process.command          (* Command to run solver process *)

  module Key = struct
    type t = Current_git.Commit.t

    let digest src =
      let json = `Assoc [
          "src", `String (Current_git.Commit.hash src);
        ]
      in
      Yojson.Safe.to_string json
  end

  module Value = struct
    type t = {
      opam_repository : Current_git.Commit.t;
      platforms : (string * Worker.Vars.t) list;
    }

    let platform_to_yojson (variant, vars) =
      `Assoc [
        "variant", `String variant;
        "vars", Worker.Vars.to_yojson vars
      ]

    let digest { opam_repository; platforms } =
      let json = `Assoc [
          "opam-repository", `String (Current_git.Commit.hash opam_repository);
          "platforms", `List (List.map platform_to_yojson platforms);
        ]
      in
      Yojson.Safe.to_string json
  end

  module Outcome = Analysis

  let id = "ci-analyse"

  let run solver job src { Value.opam_repository; platforms } =
    Current.Job.start job ~pool ~level:Current.Level.Harmless >>= fun () ->
    Current_git.with_checkout ~job src @@ fun src ->
    Current_git.with_checkout ~job opam_repository @@ fun opam_repository ->
    Analysis.of_dir ~solver ~platforms ~opam_repository ~job src

  let pp f _ = Fmt.string f "Analyse"

  let auto_cancel = true
  let latched = true
end

module Examine_cache = Current_cache.Generic(Examine)

let examine ~solver ~platforms ~opam_repository src =
  Current.component "Analyse" |>
  let> src = src
  and> opam_repository = opam_repository
  and> platforms = platforms in
  let platforms = platforms |> List.map (fun { Platform.variant; vars; _ } -> (variant, vars)) in
  Examine_cache.run solver src { Examine.Value.opam_repository; platforms }
