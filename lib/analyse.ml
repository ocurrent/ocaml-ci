open Lwt.Infix
open Current.Syntax

let pool = Current.Pool.create ~label:"analyse" 2

let is_empty_file x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_REG; st_size = 0; _ } -> true
  | _ -> false

let is_toplevel path = not (String.contains path '/')

let ( >>!= ) = Lwt_result.bind

module Analysis = struct
  type t = {
    opam_files : string list;
    ocamlformat_source : Analyse_ocamlformat.source option;
    selections : [ `Opam_build of string list | `Duniverse of string list ]; (* List of variants *)
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
    Fmt.string f vars.Platform.Vars.ocaml_version

  let of_dir ~job ~platforms dir =
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
        if is_duniverse then (
          let vs = get_ocaml_versions dir in
          if vs = [] then failwith "No OCaml compilers specified!";
          let find_compiler v =
            let v = Ocaml_version.with_just_major_and_minor (Ocaml_version.of_string_exn v) in
            let matches_compiler (_variant, vars) =
              Ocaml_version.compare v (Platform.Vars.ocaml_major_version vars) = 0
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
          Lwt.return (`Duniverse variants)
        ) else (
          Lwt.return (`Opam_build (List.map fst platforms))
        )
      end >|= fun selections ->
      let r = { opam_files; ocamlformat_source; selections } in
      Current.Job.log job "@[<v2>Results:@,%a@]" Yojson.Safe.(pretty_print ~std:true) (to_yojson r);
      Ok r
    )
end

module Examine = struct
  type t = No_context

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
      platforms : (string * Platform.Vars.t) list;
    }

    let platform_to_yojson (variant, vars) =
      `Assoc [
        "variant", `String variant;
        "vars", Platform.Vars.to_yojson vars
      ]

    let digest { platforms } =
      let json = `Assoc [
          "platforms", `List (List.map platform_to_yojson platforms);
        ]
      in
      Yojson.Safe.to_string json
  end

  module Outcome = Analysis

  let id = "ci-analyse"

  let run No_context job src { Value.platforms } =
    Current.Job.start job ~pool ~level:Current.Level.Harmless >>= fun () ->
    Current_git.with_checkout ~job src @@ fun src ->
    Analysis.of_dir ~platforms ~job src

  let pp f _ = Fmt.string f "Analyse"

  let auto_cancel = false
  let latched = true
end

module Examine_cache = Current_cache.Generic(Examine)

let examine ~platforms src =
  Current.component "Analyse" |>
  let> src = src
  and> platforms = platforms in
  let platforms = platforms |> List.map (fun { Platform.variant; vars; _ } -> (variant, vars)) in
  Examine_cache.run Examine.No_context src { Examine.Value.platforms }
