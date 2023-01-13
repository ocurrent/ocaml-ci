(** Pipeline for testing GitLab hosted repositories. *)

val local_test :
  solver:Ocaml_ci.Backend_solver.t ->
  Current_git.Local.t ->
  unit ->
  unit Current.t
(** [local_test ~solver repo] is a pipeline that tests local repository [repo]
    as the GitLab CI pipeline would. *)

val v :
  ?ocluster:Cluster_api.Raw.Client.Submission.t Capnp_rpc_lwt.Sturdy_ref.t ->
  app:Current_gitlab.Api.t ->
  solver:Ocaml_ci.Backend_solver.t ->
  migrations:string option ->
  unit ->
  unit Current.t
(** The main ocaml-ci pipeline for testing GitLab hosted repositories. 
    If [migration] is [Some path], it automatically executes the migrations.*)
