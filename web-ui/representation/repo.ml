(* module Client = Ocaml_ci_api.Client *)

type commit = { ran_for : int; status : string } [@@deriving yojson]
type repo = { name : string; history : commit list } [@@deriving yojson]
type t = repo list [@@deriving yojson]

let to_json t = Yojson.Safe.to_string @@ to_yojson t

module M = Map.Make (String)

let from_repos ~repos =
  let f { Ocaml_ci_api.Client.Repo.ran_for; status; _ } =
    let ran_for = match ran_for with None -> 0 | Some v -> int_of_float v in
    let status =
      match status with Passed -> "passed" | Failed -> "failed" | _ -> ""
    in
    { ran_for; status }
  in
  let g (name, history) =
    let history = List.map f history in
    { name; history }
  in
  List.map g repos
