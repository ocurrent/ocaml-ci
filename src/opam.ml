open Lwt.Infix
open Current.Syntax

module Find_opam = struct
  type t = No_context

  module Key = struct
    type t = Current_git.Commit.t

    let digest t = Current_git.Commit.id t
  end

  module Value = struct
    type t = string list
      [@@deriving yojson]

    let marshal t = to_yojson t |> Yojson.Safe.to_string

    let unmarshal s =
      match Yojson.Safe.from_string s |> of_yojson with
      | Ok x -> x
      | Error e -> failwith e
  end

  let id = "find-opam"

  let build ~switch ~set_running No_context job src =
    set_running ();
    Current_git.with_checkout ~switch ~job src @@ fun tmpdir ->
    let cmd = "", [| "find"; "-name"; "*.opam" |] in 
    Current.Process.check_output ~cwd:tmpdir ~switch ~job cmd >|= Stdlib.Result.map @@ fun output ->
    let files = String.split_on_char '\n' output |> List.filter ((<>) "") in
    Current.Job.log job "Found: %a" Fmt.(Dump.list (quote string)) files;
    files

  let pp f _ = Fmt.string f "**/*.opam"

  let auto_cancel = false

  let level _ _ = Current.Level.Harmless
end

module Find_opam_cache = Current_cache.Make(Find_opam)

let find_opam_files src =
  Current.component "**/*.opam" |>
  let> src = src in
  Find_opam_cache.get Find_opam.No_context src
