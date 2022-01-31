type t [@@deriving eq,ord,yojson]

val v : arch:Ocaml_version.arch ->
        distro:string -> ocaml_version:Ocaml_version.t ->
        opam_version:Opam_version.t ->
        (t, [> `Msg of string ]) result

val arch : t -> Ocaml_version.arch
val distro : t -> string
val ocaml_version : t -> Ocaml_version.t
val with_ocaml_version : Ocaml_version.t -> t -> t
val opam_version : t -> Opam_version.t

val id : t -> string
val docker_tag : t -> string
val pp : t Fmt.t

val to_string : t -> string
val of_string : string -> t
