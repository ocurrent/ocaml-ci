(** Build and test all the opam packages in a given build context.
    [~variant] is the variant of the "ocurrent/opam" image.
    [~repo] is the ID of the repository-under-test on GitHub. *)
val v :
  docker:(module S.DOCKER_CONTEXT with type source = 'source) ->
  schedule:Current_cache.Schedule.t ->
  variant:string ->
  pkg:string ->
  'source ->
  [> `Built ] Current.t
