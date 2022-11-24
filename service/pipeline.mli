val local_test :
  solver:Ocaml_ci.Backend_solver.t ->
  Current_git.Local.t ->
  unit ->
  unit Current.t
(** [local_test ~solver repo] is a pipeline that tests local repository [repo]
    as the CI would. *)

val v :
  ?ocluster:Cluster_api.Raw.Client.Submission.t Capnp_rpc_lwt.Sturdy_ref.t ->
  app:Current_github.App.t ->
  solver:Ocaml_ci.Backend_solver.t ->
  migrations:string option ->
  unit ->
  unit Current.t
(** The main ocaml-ci pipeline. Tests everything configured for GitHub
    application [app]. If [migration] is [Some path], it will automatically
    executes the migrations. *)
