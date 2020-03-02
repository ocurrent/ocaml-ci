module Make (Docker : S.DOCKER_CONTEXT) : sig
  (** Build and test all the opam packages in a given build context.
      [~variant] is the variant of the "ocurrent/opam" image.
      [~repo] is the ID of the repository-under-test on GitHub. *)
  val v :
    schedule:Current_cache.Schedule.t ->
    variant:string ->
    pkg:string ->
    Docker.source ->
    Docker.image Current.t
end
