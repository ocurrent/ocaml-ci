val main : Remote_commit.t list -> unit
(** [main hash] runs a worker process that reads requests from stdin and writes results to stdout,
    using the given commit(s) from opam-repository repos. *)
