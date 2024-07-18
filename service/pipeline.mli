(** Pipeline for testing GitHub hosted repositories. *)

val local_test :
  conn:Current_ocluster.Connection.t ->
  solver:Ocaml_ci.Backend_solver.t ->
  Current_git.Local.t ->
  unit ->
  unit Current.t
(** [local_test ~solver repo] is a pipeline that tests local repository [repo]
    as the GitHub CI pipeline would. *)

val v :
  ?ocluster:Cluster_api.Raw.Client.Submission.t Capnp_rpc_lwt.Sturdy_ref.t ->
  app:Current_github.App.t ->
  conn:Current_ocluster.Connection.t ->
  solver:Ocaml_ci.Backend_solver.t ->
  migrations:string option ->
  unit ->
  unit Current.t
(** The main ocaml-ci pipeline for testing GitHub hosted repositories. Tests
    everything configured for GitHub application [app]. If [migration] is
    [Some path], it automatically executes the migrations. *)
