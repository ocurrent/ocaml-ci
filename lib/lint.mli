module Make (Docker : S.DOCKER_CONTEXT) : sig

  val v :
    pull_schedule:Current_cache.Schedule.t ->
    analysis:Analyse.Analysis.t Current.t ->
    source:Docker.source ->
    [> `Checked | `Check_skipped ] Current.t
  (** [v ~analysis ~src] runs the linting step:
      - Ensures OCamlformat is installed depending on {!Analysis.ocamlformat_source}.
      - Checks formatting using "dune build @fmt".
      The base image [base] should have OPAM installed. *)

end
