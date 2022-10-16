(** A platform on which we wish to perform test builds. *)

type base = [ `Docker of Current_docker.Raw.Image.t | `MacOS of string ]

val to_yojson : base -> Yojson.Safe.t
val to_string : base -> string
val base_pp : Format.formatter -> base -> unit

type t = {
  label : string;
  builder : Builder.t;
  pool : string; (* OCluster pool *)
  variant : Variant.t; (* e.g. "debian-10-ocaml-4.08" *)
  base : base; (* Base image to use *)
  vars : Ocaml_ci_api.Worker.Vars.t;
}

val pp : t Fmt.t
val compare : t -> t -> int

val compiler_matches_major_and_minor :
  Ocaml_ci_api.Worker.Vars.t -> version:Ocaml_version.t -> bool
(** [compiler_matches_major_and_minor vars ~version] is [true] iff the compiler
    version in [vars] matches [version], considering only the major and minor
    parts of the version number. *)

val set_compiler_version :
  Ocaml_ci_api.Worker.Vars.t ->
  version:Ocaml_version.t ->
  Ocaml_ci_api.Worker.Vars.t

val get :
  arch:Ocaml_version.arch ->
  label:string ->
  builder:Builder.t ->
  pool:string ->
  distro:string ->
  ocaml_version:Ocaml_version.t ->
  host_base:Current_docker.Raw.Image.t Current.t ->
  opam_version:Opam_version.t ->
  Current_docker.Raw.Image.t Current.t ->
  t Current.t
(** [get ~label ~builder ~variant ~host_base base] creates a [t] by getting the
    opam variables from [host_base] and returning [base] for subsequent builds. *)

val pull :
  arch:Ocaml_version.arch ->
  schedule:Current_cache.Schedule.t ->
  builder:Builder.t ->
  distro:string ->
  ocaml_version:Ocaml_version.t ->
  opam_version:Opam_version.t ->
  Current_docker.Raw.Image.t Current.t
(** [pull ~schedule ~builder ~distro ~ocaml_version] pulls
    "ocaml/opam:\{distro\}-ocaml-\{version\}" on [schedule]. *)

val get_macos :
  arch:Ocaml_version.arch ->
  label:string ->
  builder:Builder.t ->
  pool:string ->
  distro:string ->
  ocaml_version:Ocaml_version.t ->
  opam_version:Opam_version.t ->
  [< `MacOS of string ] Current.t ->
  t Current.t
(** [get_macos ~label ~builder ~variant ~host_base base] creates a [t] by
    getting the opam variables from [host_base] and returning [base] for
    subsequent builds. *)
