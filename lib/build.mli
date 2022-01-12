(** Build and test all the opam packages in a given build context on the given platform.
    [~repo] is the ID of the repository-under-test on GitHub. *)
val v :
  platforms:Platform.t list Current.t ->
  repo:Repo_id.t Current.t ->
  spec:Spec.t Current.t ->
  Current_git.Commit.t Current.t ->
  ([> `Built | `Checked ] Current_term.Output.t * Current.job_id option) Current.t

val make_build_spec :
  base:Current_docker.Raw.Image.t ->
  repo:Repo_id.t ->
  variant:Variant.t ->
  ty:Spec.ty ->
  Obuilder_spec.t
