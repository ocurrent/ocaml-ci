(** Build and test all the opam packages in a given build context on the given platform.
    [~repo] is the ID of the repository-under-test on GitHub. *)
val v :
  platform:Platform.t Current.t ->
  schedule:Current_cache.Schedule.t ->
  repo:Current_github.Repo_id.t Current.t ->
  analysis:Analyse.Analysis.t ->
  revdep:string option ->
  with_tests:bool ->
  pkg:string ->
  Current_git.Commit.t Current.t ->
  Current_docker.Raw.Image.t Current.t

val pread :
  platform:Platform.t Current.t ->
  Current_docker.Raw.Image.t ->
  args:string list ->
  string Current.t
