(** Helper functions for generating stub projects under test **)

type contents = Format.formatter -> unit

type file = Folder of string * file list | File of string * contents

val opam : contents
(** Contents of an example [.opam] file *)

val ocamlformat : version:string -> contents
(** Contents of a [.ocamlformat] file with a particular version *)

val instantiate : root:string -> file list -> unit
(** Take a directory specification and persist it to disk *)
