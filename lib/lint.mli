val dockerfile : base:string -> info:Analyse.Analysis.t -> variant:string -> for_user:bool -> Dockerfile.t
(** A Dockerfile that checks the formatting.
    - Ensures OCamlformat is installed depending on {!Analysis.ocamlformat_source}.
    - Checks formatting using "dune build @fmt". *)
