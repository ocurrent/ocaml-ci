val v :
  builder:Builder.t ->
  schedule:Current_cache.Schedule.t ->
  analysis:Analyse.Analysis.t Current.t ->
  source:Current_git.Commit.t Current.t ->
  [> `Checked ] Current.t
(** [v ~builder ~schedule ~analysis ~source] runs the linting step:
    - Ensures OCamlformat is installed depending on {!Analysis.ocamlformat_source}.
    - Checks formatting using "dune build @fmt". *)
