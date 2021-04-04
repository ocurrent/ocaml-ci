type t = {
  repo : string;
  hash : string
} [@@deriving yojson]

let v ~repo ~hash =
  { repo; hash }

let repo x = x.repo
let hash x = x.hash

let pp f {repo; hash} =
  Fmt.pf f "%s (%s)" repo hash

let to_string checkout =
  to_yojson checkout |> Yojson.Safe.to_string

let of_string str =
  Yojson.Safe.from_string str |> of_yojson

let list_to_string checkouts =
  `List (List.map to_yojson checkouts) |> Yojson.Safe.to_string

let of_yojson_or_fail json =
  match of_yojson json with
  | Ok x -> x
  | Error ex -> failwith ex

let list_of_string_or_fail str =
  List.map of_yojson_or_fail Yojson.Safe.(from_string str |> Util.to_list)
