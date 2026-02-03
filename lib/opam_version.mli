(** Opam versions supported. *)

type t = [ `V2_0 | `V2_1 | `V2_2 | `V2_3 | `V2_4 | `V2_5 ] [@@deriving ord, yojson, eq]

val pp : t Fmt.t
val to_string : t -> string
val to_string_with_patch : t -> string
val default : t
val of_string : string -> (t, [ `Msg of string ]) result
