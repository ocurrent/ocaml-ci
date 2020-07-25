type t [@@deriving eq,ord,yojson]

val v : arch:Ocaml_version.arch option -> string -> t

val arch : t -> Ocaml_version.arch option
val id : t -> string
val pp : t Fmt.t

val to_string : t -> string
val of_string : string -> t

(** [to_opam_arch t] outputs a string suitable for use in opam files as the [%{arch}%] variable *)
val to_opam_arch : Ocaml_version.arch option -> string option

(** [to_docker_arch t] outputs a string suitable for mapping to docker multiarch manifests *)
val to_docker_arch : Ocaml_version.arch option -> string option
