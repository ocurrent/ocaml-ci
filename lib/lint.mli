module Make (Docker : S.DOCKER_CONTEXT) : sig

  val v :
    analysis:Analyse.Analysis.t Current.t ->
    src:Current_git.Commit.t Current.t ->
    [> `Checked | `Check_skipped ] Current.t
  (** [v ~analysis ~src] runs the linting step:
      - Checks formatting using "dune build @fmt".
        Ensures OCamlformat is installed depending on [Analysis.ocamlformat_source].
      The base image [base] should have OPAM installed. *)

end
