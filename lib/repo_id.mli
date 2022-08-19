type t = { owner : string; name : string }
(** Git source repository *)

val pp : t Fmt.t
val compare : t -> t -> int
