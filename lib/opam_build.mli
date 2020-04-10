module Make (Docker : S.DOCKER_CONTEXT) : sig
  type t

  val base :
    schedule:Current_cache.Schedule.t ->
    variant:string ->
    t

  (** Build and test all the opam packages in a given build context.
      [~variant] is the variant of the "ocurrent/opam" image.
      [~repo] is the ID of the repository-under-test on GitHub. *)
  val v :
    revdep:string option ->
    with_tests:bool ->
    pkg:string ->
    Docker.source ->
    t ->
    Docker.image Current.t
end
