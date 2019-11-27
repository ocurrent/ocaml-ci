module Docker = Conf.Builder_amd1

type ocamlformat_version = [
  | `Vendored (** OCamlformat is vendored, don't install it via opam *)
  | `Version of string (** Which version of OCamlformat to use *)
]

val ocamlformat :
  ocamlformat_version:ocamlformat_version Current.t ->
  base:Docker.Image.t Current.t ->
  src:Current_git.Commit.t Current.t ->
  unit Current.t
(** [ocamlformat ~ocamlformat_version ~base ~src] runs an OCamlformat check
    using Dune. See [ocamlformat_version] for details. *)
