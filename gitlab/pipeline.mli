val local_test :
  solver:Ocaml_ci_api.Solver.t -> Current_git.Local.t -> unit -> unit Current.t
(** [local_test ~solver repo] is a pipeline that tests local repository [repo]
    as the CI would. *)

val v :
  ?ocluster:Cluster_api.Raw.Client.Submission.t Capnp_rpc_lwt.Sturdy_ref.t ->
  app:Current_gitlab.Api.t ->
  solver:Ocaml_ci_api.Solver.t ->
  migrations:string option ->
  unit ->
  unit Current.t
(** The main ocaml-ci pipeline. Tests everything configured for GitLab.
    If [migration] is [Some path], it will automatically
    executes the migrations. *)
