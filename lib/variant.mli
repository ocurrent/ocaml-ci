(** Variants of builds to perform.

    A build variant covers the OCaml version, opam version, hardware [arch] and
    operating system distribution. *)

type t [@@deriving eq, ord, yojson]

val v :
  arch:Ocaml_version.arch ->
  distro:string ->
  ocaml_version:Ocaml_version.t ->
  opam_version:Opam_version.t ->
  (t, [> `Msg of string ]) result

val arch : t -> Ocaml_version.arch
val distro : t -> string
val distro' : t -> Obuilder_spec_opam.Distro.t option
val ocaml_version : t -> Ocaml_version.t
val with_ocaml_version : Ocaml_version.t -> t -> t
val opam_version : t -> Opam_version.t
val id : t -> string

val docker_tag : t -> string
(** Print [t] as a docker tag. *)

val pp : t Fmt.t
val to_string : t -> string
val of_string : string -> t
val os : t -> Obuilder_spec_opam.Distro.os_family
