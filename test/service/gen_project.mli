(** Helper functions for generating stub projects under test **)

type contents = Format.formatter -> unit
type file = Folder of string * file list | File of string * contents

val file : string -> contents -> file
val folder : string -> file list -> file

val opam_monorepo_spec_file : contents
(** Contents of an example [.opam] file for opam-monorepo *)

val opam_monorepo_lock_file : monorepo_version:string option -> contents
(** Contents of an example [.opam.locked] file for opam-monorepo.
    [monorepo_version] will populate a [x-opam-monorepo-version] field. *)

val opam : ?ocaml:string -> contents
(** Contents of an example [.opam] file

    @param ocaml Version constraint on OCaml. *)

val ocamlformat : version:string -> contents
(** Contents of a [.ocamlformat] file with a particular version *)

val empty_file : contents
(** An empty file *)

val instantiate : root:string -> file list -> unit
(** Take a directory specification and persist it to disk *)

(** {2 Generation of opam-repository} *)

val dummy_package : string -> string list -> file
(** [dummy_package name versions] is an opam-repository "packages/$name"
    directory, containing dummy packages for each version. *)
