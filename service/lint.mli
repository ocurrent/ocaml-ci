module Docker = Conf.Builder_amd1

val ocamlformat :
  ocamlformat_source:Ocaml_ci.Analyse_ocamlformat.source Current.t ->
  base:Docker.Image.t Current.t ->
  src:Current_git.Commit.t Current.t ->
  unit Current.t
(** [ocamlformat ~ocamlformat_source ~base ~src] runs an OCamlformat check
    using Dune. [ocamlformat_source] tells if OCamlformat is vendored or should
    be installed from Opam. The base image [base] should have OPAM installed. *)
