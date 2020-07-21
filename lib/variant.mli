type t [@@deriving eq,ord,yojson]

val v : arch:Ocaml_version.arch option -> string -> t

val arch : t -> Ocaml_version.arch option
val id : t -> string
val pp : t Fmt.t

val to_string : t -> string
val of_string : string -> t

(** [to_opam_arch t] is either an opam-style architecture string, or the [%{arch}%] variable to be expanded later. *)
val to_opam_arch : Ocaml_version.arch option -> string

(** [to_docker_arch t] outputs a string suitable for mapping to docker multiarch manifests *)
val to_docker_arch : Ocaml_version.arch option -> string option
