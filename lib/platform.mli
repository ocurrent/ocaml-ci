(** A platform on which we wish to perform test builds. *)

(** Opam variables. *)
module Vars : sig
  type t = {
    arch : string;
    os : string;
    os_family : string;
    os_distribution : string;
    os_version : string;
  } [@@deriving yojson]
end

type t = {
  label : string;
  builder : Builder.t;
  variant : string;
  base : Current_docker.Raw.Image.t;
  vars : Vars.t;
}

val pp : t Fmt.t
val compare : t -> t -> int

val get :
  label:string ->
  builder:Builder.t ->
  variant:string ->
  Current_docker.Raw.Image.t Current.t ->
  t Current.t
(** [get ~label ~builder ~variant base] creates a [t] by getting the opam variables from [base]. *)
