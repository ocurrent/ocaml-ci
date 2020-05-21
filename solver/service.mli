val v : pool:Lwt_process.process Lwt_pool.t -> Ocaml_ci_api.Solver.t
(** [v ~pool] is a solver service that distributes work to workers in [pool]. *)
