type t = [ `V2_0 | `V2_1 | `V2_2 | `V2_3 | `V2_4 ] [@@deriving ord, yojson, eq]

let to_string = function
  | `V2_0 -> "2.0"
  | `V2_1 -> "2.1"
  | `V2_2 -> "2.2"
  | `V2_3 -> "2.3"
  | `V2_4 -> "2.4"

let to_string_with_patch = function
  | `V2_0 -> "2.0.10"
  | `V2_1 -> "2.1.6"
  | `V2_2 -> "2.2.1"
  | `V2_3 -> "2.3.0"
  | `V2_4 -> "2.4.1"

let pp = Fmt.of_to_string to_string
let default = `V2_0

let of_string = function
  | "2.0" -> Ok `V2_0
  | "2.1" -> Ok `V2_1
  | "2.2" -> Ok `V2_2
  | "2.3" -> Ok `V2_3
  | "2.4" -> Ok `V2_4
  | s -> Error (`Msg (s ^ ": invalid opam version"))
