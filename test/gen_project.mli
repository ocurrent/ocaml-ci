(** Helper functions for generating stub projects under test **)

type contents = Format.formatter -> unit

type file = Folder of string * file list | File of string * contents

val dune_get : contents
(** Contents of an example [dune-get] file *)

val opam : contents
(** Contents of an example [.opam] file *)

val ocamlformat : version:string -> contents
(** Contents of a [.ocamlformat] file with a particular version *)

val empty_file : contents
(** An empty file *)

val instantiate : root:string -> file list -> unit
(** Take a directory specification and persist it to disk *)
