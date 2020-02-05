(** Generate a Dockerfile for building all the opam packages in the build context. *)
val dockerfile :
  base:string ->
  pkg:string ->
  variant:string ->
  Dockerfile.t
