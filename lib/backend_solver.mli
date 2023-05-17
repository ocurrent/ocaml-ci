(** Solver service connections. *)

type t
(** Type of a solver service, either as a local solver or a remote solver
    cluster. *)

val v : Uri.t option -> t
(** Create a solver service from [Uri.t option].

    For Some uri a remote CapnP connection is setup to a solver pool. If the
    option is None a local solver will be created. *)

val local_ci : t -> Solver_worker.Solver_request.t Lwt.t
(** Retrieve the local solver if [t] is a local solver, otherwise fail with an
    exception. *)

val solve :
  t ->
  Current.Job.t ->
  Ocaml_ci_api.Worker.Solve_request.t ->
  log:Ocaml_ci_api.Raw.Solve.Builder.Log.t Capnp_rpc_lwt.Capability.t ->
  Ocaml_ci_api.Worker.Solve_response.t Lwt.t
(** [solve t] run a solve request against solver [t].

    Returns a [Ocaml_ci_api.Worker.Solve_response.t] on success, otherwise fails
    with an exception. *)
