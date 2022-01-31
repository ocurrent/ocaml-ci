type t = [`V2_0 | `V2_1] [@@deriving ord,yojson, eq]

let to_string = function `V2_0 -> "2.0" | `V2_1 ->  "2.1"

let to_string_with_patch = function `V2_0 -> "2.0.10" | `V2_1 ->  "2.1.2"

let pp = Fmt.of_to_string to_string

let default = `V2_0

let of_string = function
  | "2.0" -> Ok `V2_0
  | "2.1" -> Ok `V2_1
  | s -> Error (`Msg (s ^ ": invalid opam version"))
