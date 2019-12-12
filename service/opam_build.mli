(** Generate a Dockerfile for building all the opam packages in the build context. *)
val dockerfile :
  base:string ->
  info:Ocaml_ci.Analyse.Analysis.t ->
  repo:Current_github.Repo_id.t ->
  variant:string ->
  Dockerfile.t
