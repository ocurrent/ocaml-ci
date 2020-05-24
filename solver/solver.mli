val main : Git_unix.Store.Hash.t -> unit
(** [main hash] runs a worker process that reads requests from stdin and writes results to stdout,
    using commit [hash] in opam-repository. *)
