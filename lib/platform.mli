(** A platform on which we wish to perform test builds. *)

(** Opam variables. *)
module Vars : sig
  type t = {
    arch : string;
    os : string;
    os_family : string;
    os_distribution : string;
    os_version : string;
    ocaml_version : string;
  } [@@deriving yojson]

  val ocaml_major_version : t -> Ocaml_version.t
end

type t = {
  label : string;
  builder : Builder.t;
  variant : string;                     (* e.g. "debian-10-ocaml-4.08" *)
  base : Current_docker.Raw.Image.t;
  vars : Vars.t;
}

val pp : t Fmt.t
val compare : t -> t -> int

val get :
  label:string ->
  builder:Builder.t ->
  distro:string ->
  ocaml_version:string ->
  Current_docker.Raw.Image.t Current.t ->
  t Current.t
(** [get ~label ~builder ~variant base] creates a [t] by getting the opam variables from [base]. *)

val pull :
  schedule:Current_cache.Schedule.t ->
  builder:Builder.t ->
  distro:string ->
  ocaml_version:string ->
  Current_docker.Raw.Image.t Current.t
(** [pull ~schedule ~builder ~distro ~ocaml_version] pulls "ocurrent/opam:{distro}-ocaml-{version}" on [schedule]. *)
