module Docker = Conf.Builder_amd1

val v_from_opam :
  ocamlformat_version:string Current.t ->
  base:Docker.Image.t Current.t ->
  src:Current_git.Commit.t Current.t ->
  unit Current.t
(** [v ~ocamlformat_version ~base ~src] runs a Dune linting check on [src] via
    the base image [base] with OPAM installed. *)
