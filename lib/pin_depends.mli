val get_opam : job:Current.Job.t -> pkg:OpamPackage.t -> OpamUrl.t -> string Lwt.t
(** [pkg] should be pinned to [url]; return the opam file. *)
