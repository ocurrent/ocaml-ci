open Lwt.Infix
open Current.Syntax

module Examine = struct
  type t = No_context

  module Key = struct
    type t = Current_git.Commit.t

    let digest t = Current_git.Commit.id t
  end

  module Value = struct
    type t = {
      opam_files : string list;
    }
    [@@deriving yojson]

    let marshal t = to_yojson t |> Yojson.Safe.to_string

    let unmarshal s =
      match Yojson.Safe.from_string s |> of_yojson with
      | Ok x -> x
      | Error e -> failwith e

    let opam_files t = t.opam_files
  end

  let id = "ci-analyse"

  let build ~switch No_context job src =
    Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
    Current_git.with_checkout ~switch ~job src @@ fun tmpdir ->
    let cmd = "", [| "find"; "-name"; "*.opam" |] in
    Current.Process.check_output ~cwd:tmpdir ~switch ~job cmd >|= Stdlib.Result.map @@ fun output ->
    let opam_files = String.split_on_char '\n' output |> List.filter ((<>) "") in
    Current.Job.log job "Found: %a" Fmt.(Dump.list (quote string)) opam_files;
    { Value.opam_files }

  let pp f _ = Fmt.string f "Analyse"

  let auto_cancel = false
end

module Analysis = Examine.Value

module Examine_cache = Current_cache.Make(Examine)

let examine src =
  Current.component "Analyse" |>
  let> src = src in
  Examine_cache.get Examine.No_context src
