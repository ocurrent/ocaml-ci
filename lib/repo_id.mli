(** Git Forge agnostic representation of a Repository identifier. *)

type t = { owner : string; name : string }
(** Git source repository *)

val pp : t Fmt.t
(** Pretty print [t]. *)

val compare : t -> t -> int
(** Compare [t]'s. *)
