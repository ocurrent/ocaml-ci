val v :
  n_workers:int ->
  create_worker:(Git_unix.Store.Hash.t -> Lwt_process.process) ->
  Ocaml_ci_api.Solver.t Lwt.t
(** [v ~n_workers ~create_worker] is a solver service that distributes work to up to
    [n_workers] subprocesses, using [create_worker hash] to spawn new workers. *)
