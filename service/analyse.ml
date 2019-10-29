open Lwt.Infix
open Current.Syntax

let is_directory x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_DIR; _ } -> true
  | _ -> false
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false

let is_empty_file x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_REG; st_size = 0; _ } -> true
  | _ -> false

module Examine = struct
  type t = No_context

  module Key = struct
    type t = Current_git.Commit.t

    let digest t = Current_git.Commit.id t
  end

  module Value = struct
    type t = {
      is_duniverse : bool;
      opam_files : string list;
      ocamlformat_version : string option;
    }
    [@@deriving yojson]

    let marshal t = to_yojson t |> Yojson.Safe.to_string

    let unmarshal s =
      match Yojson.Safe.from_string s |> of_yojson with
      | Ok x -> x
      | Error e -> failwith e

    let opam_files t = t.opam_files

    let is_duniverse t = t.is_duniverse

    let ocamlformat_version t = t.ocamlformat_version
  end

  let id = "ci-analyse"

  let ocamlformat_version_from_string =
    let re =
      Re.(
        seq
          [
            start;
            rep space;
            str "version";
            rep space;
            char '=';
            rep space;
            group (rep1 @@ diff graph (set "#"));
            rep space;
            eol;
          ]
        |> compile)
    in
    fun path ->
      Re.exec_opt re path |> function
      | Some g -> Some (Re.Group.get g 1)
      | None -> None

  let ocamlformat_version_from_file job path =
    let ( let+ ) = Lwt.Infix.( >|= ) in
    if not (Sys.file_exists path) then
      let () = Current.Job.log job "No .ocamlformat file found" in
      Lwt.return (Ok None)
    else
      let+ versions = Lwt_io.with_file ~mode:Lwt_io.input path (fun channel ->
          Lwt_io.read_lines channel
          |> Lwt_stream.filter_map ocamlformat_version_from_string
          |> Lwt_stream.to_list
        )
      in
      match versions with
      | [ v ] ->
          let () =
            Current.Job.log job "Found OCamlformat version '%s' in dotfile" v
          in
          Ok (Some v)
      | _ -> Error (`Msg "Unable to parse .ocamlformat file")

  let get_ocamlformat_version job root =
    Fpath.(to_string (root / ".ocamlformat")) |> ocamlformat_version_from_file job
    >|= function
    | Ok result -> result
    | Error (`Msg e) -> failwith e

  let build ~switch No_context job src =
    Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
    Current_git.with_checkout ~switch ~job src @@ fun tmpdir ->
    let is_duniverse = is_directory (Filename.concat (Fpath.to_string tmpdir) "duniverse") in
    get_ocamlformat_version job tmpdir >>= fun ocamlformat_version ->
    let cmd = "", [| "find"; "-name"; "*.opam" |] in
    Current.Process.check_output ~cwd:tmpdir ~switch ~job cmd >|= Stdlib.Result.map @@ fun output ->
    let opam_files =
      String.split_on_char '\n' output
      |> List.sort String.compare
      |> List.filter (function
          | "" -> false
          | path ->
            let full_path = Filename.concat (Fpath.to_string tmpdir) path in
            if is_empty_file full_path then (
              Current.Job.log job "WARNING: ignoring empty opam file %S" path;
              false
            ) else
              true
        )
    in
    let r = { Value.opam_files; is_duniverse; ocamlformat_version } in
    Current.Job.log job "@[<v2>Results:@,%a@]" Yojson.Safe.(pretty_print ~std:true) (Value.to_yojson r);
    r

  let pp f _ = Fmt.string f "Analyse"

  let auto_cancel = false
end

module Analysis = Examine.Value

module Examine_cache = Current_cache.Make(Examine)

let examine src =
  Current.component "Analyse" |>
  let> src = src in
  Examine_cache.get Examine.No_context src
