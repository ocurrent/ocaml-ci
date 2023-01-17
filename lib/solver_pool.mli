(** Local solver service pool. *)

val spawn_local : ?solver_dir:string -> unit -> Ocaml_ci_api.Solver.t
(** Spawns a local solver service with a pool of workers.

    Supports Linux, macOS and Windows. *)
