(** Generate a Dockerfile for building all the opam packages in the build context. *)
val dockerfile :
  base:string ->
  info:Analyse.Analysis.t ->
  repo:Current_github.Repo_id.t ->
  Dockerfile.t
