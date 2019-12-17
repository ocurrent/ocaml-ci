type version = Version of string | Vendored of string

val get_ocamlformat_version : opam_files:string list -> Current.Job.t -> Fpath.t -> version option Lwt.t
