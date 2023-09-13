type t = [ `V2_0 | `V2_1 | `V2_2 ] [@@deriving ord, yojson, eq]

let to_string = function
  | `V2_0 -> "2.0"
  | `V2_1 -> "2.1"
  | `V2_2 -> "2.2" (* TODO Should be 2.2 when an official release is made. *)

let to_string_with_patch = function
  | `V2_0 -> "2.0.10"
  | `V2_1 -> "2.1.5"
  | `V2_2 -> "2.2.0~alpha2"

let pp = Fmt.of_to_string to_string
let default = `V2_1

let of_string = function
  | "2.0" -> Ok `V2_0
  | "2.1" -> Ok `V2_1
  | "2.2" | "2.2.0~alpha2" -> Ok `V2_2
  | s -> Error (`Msg (s ^ ": invalid opam version"))
