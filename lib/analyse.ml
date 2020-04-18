open Lwt.Infix
open Current.Syntax

type key = {
  src : Current_git.Commit.t;
  master : Current_git.Commit.t;
}

let pool = Current.Pool.create ~label:"analyse" 2

let ( >>!= ) = Lwt_result.bind

module Analysis = struct
  type t = {
    opam_files : string list;
  }
  [@@deriving yojson]

  let marshal t = to_yojson t |> Yojson.Safe.to_string

  let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with
    | Ppx_deriving_yojson_runtime.Result.Ok x -> x
    | Ppx_deriving_yojson_runtime.Result.Error _ -> failwith "lol"

  let opam_files t = t.opam_files

  let is_duniverse _ = false

  let ocamlformat_source _ = None

  let of_dir ~master ~head ~job dir =
    (* TODO: Check if the PR added an opam file in packages/<pkg> instead of packages/<pkg>/<pkg>.<ver> (common mistake) *)
    (* TODO: Split modified vs. added (using git diff --name-status) *)
    let master = Current_git.Commit.id master in
    let head = Current_git.Commit.id head in
    let fmt = Printf.sprintf in
    Current.Process.exec ~cwd:dir ~cancellable:true ~job ("", [|"git";"checkout";"-b";"opam-ci__cibranch";master|]) >>!= fun () ->
    Current.Process.exec ~cwd:dir ~cancellable:true ~job ("", [|"git";"merge";head|]) >>!= fun () ->
    let cmd = "", [| "sh"; "-c"; fmt {|git diff %s | sed -E -n -e 's,^\+\+\+ b/packages/[^/]*/([^/]*)/.*,\1,p'|} master |] in
    Current.Process.check_output ~cwd:dir ~cancellable:true ~job cmd >>!= fun output ->
    let opam_files =
      String.split_on_char '\n' output
      |> List.filter (fun s -> not (String.equal s ""))
      |> List.sort String.compare
    in
    let r = { opam_files } in
    Current.Job.log job "@[<v2>Results:@,%a@]" Yojson.Safe.(pretty_print ~std:true) (to_yojson r);
    Lwt.return (Ok r)
end

module Examine = struct
  type t = No_context

  module Key = struct
    type t = key

    let digest {src; master = _} =
      (* Only keep track of [src]. We don't want to redo the whole pipeline when something is pushed on master *)
      Current_git.Commit.id src
  end

  module Value = Analysis

  let id = "ci-analyse"

  let build No_context job {src; master} =
    Current.Job.start job ~pool ~level:Current.Level.Harmless >>= fun () ->
    Current_git.with_checkout ~enable_submodules:false ~job src (Analysis.of_dir ~master ~head:src ~job)

  let pp f _ = Fmt.string f "Analyse"

  let auto_cancel = false
end

module Examine_cache = Current_cache.Make(Examine)

let examine ~master src =
  Current.component "Analyse" |>
  let> src = src
  and> master = master in
  Examine_cache.get Examine.No_context {src; master}
