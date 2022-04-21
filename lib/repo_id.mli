(** Git source repository *)
type t = {
  owner : string;
  name : string;
}

val pp : t Fmt.t

val compare : t -> t -> int
