(** Build and test all the opam packages in a given build context.
    [~variant] is the variant of the "ocurrent/opam" image.
    [~repo] is the ID of the repository-under-test on GitHub. *)
val v :
  docker:(module S.DOCKER_CONTEXT with type source = 'source) ->
  pull_schedule:Current_cache.Schedule.t ->
  variant:string ->
  repo:Current_github.Repo_id.t Current.t ->
  analysis:Analyse.Analysis.t Current.t ->
  'source ->
  [> `Built ] Current.t
