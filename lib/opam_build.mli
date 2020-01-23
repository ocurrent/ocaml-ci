(** Build and test all the opam packages in a given build context.
    [~variant] is the variant of the "ocurrent/opam" image.
    [~repo] is used to identify a build cache for duniverse projects. *)
val v :
  docker:(module S.DOCKER_CONTEXT with type source = 'source) ->
  variant:string ->
  repo:Current_github.Repo_id.t Current.t ->
  analysis:Analyse.Analysis.t Current.t ->
  'source ->
  [> `Built ] Current.t
