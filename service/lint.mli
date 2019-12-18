module Docker = Conf.Builder_amd1

val v_fmt :
  ocamlformat_source:Ocaml_ci.Analyse_ocamlformat.source Current.t ->
  base:Docker.Image.t Current.t ->
  src:Current_git.Commit.t Current.t ->
  unit Current.t
(** [v_fmt ~ocamlformat_source ~base ~src] ensures OCamlformat is installed and
    runs "dune build @fmt". [ocamlformat_source] tells if OCamlformat is
    vendored or should be installed from Opam.
    The base image [base] should have OPAM installed. *)
