type t

val create : ?solver_dir:string -> Uri.t option -> t
val local : ?solver_dir:string -> unit -> t
val local_ci : t -> Ocaml_ci_api.Solver.t Lwt.t

val solve :
  t ->
  Current.Job.t ->
  Ocaml_ci_api.Worker.Solve_request.t ->
  log:Ocaml_ci_api.Raw.Solve.Builder.Log.t Capnp_rpc_lwt.Capability.t ->
  Ocaml_ci_api.Worker.Solve_response.t Lwt.t
