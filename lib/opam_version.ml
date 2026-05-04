type t = [ `V2_1 | `V2_2 | `V2_3 | `V2_4 | `V2_5 ] [@@deriving ord, yojson, eq]

let to_string = function
  | `V2_1 -> "2.1"
  | `V2_2 -> "2.2"
  | `V2_3 -> "2.3"
  | `V2_4 -> "2.4"
  | `V2_5 -> "2.5"

let pp = Fmt.of_to_string to_string

let of_string = function
  | "2.1" -> Ok `V2_1
  | "2.2" -> Ok `V2_2
  | "2.3" -> Ok `V2_3
  | "2.4" -> Ok `V2_4
  | "2.5" -> Ok `V2_5
  | s -> Error (`Msg (s ^ ": invalid opam version"))
