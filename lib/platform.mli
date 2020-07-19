(** A platform on which we wish to perform test builds. *)

type t = {
  label : string;
  builder : Builder.t;
  pool : string;        (* OCluster pool *)
  variant : string;                     (* e.g. "debian-10-ocaml-4.08" *)
  base : Current_docker.Raw.Image.t;
  vars : Ocaml_ci_api.Worker.Vars.t;
}

val pp : t Fmt.t
val compare : t -> t -> int

val get :
  label:string ->
  builder:Builder.t ->
  pool:string ->
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
