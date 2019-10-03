(** Generate a Dockerfile for building all the opam packages in the build context. *)
val dockerfile : base:Current_docker.Default.Image.t -> info:Analyse.Analysis.t -> Dockerfile.t
