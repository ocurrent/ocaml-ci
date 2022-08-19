type t = [ `V2_0 | `V2_1 ] [@@deriving ord, yojson, eq]

val pp : t Fmt.t
val to_string : t -> string
val to_string_with_patch : t -> string
val default : t
val of_string : string -> (t, [ `Msg of string ]) result
