type t
(** A Git clone of opam-repository. *)

val of_dir : string -> t
(** [of_dir root] uses the opam repository Git clone at [root]. *)

val packages_dir : t -> string
(** [packages_dir t] is the path of [t]'s "packages" directory. *)

val oldest_commit_with : t -> OpamPackage.t list -> string Lwt.t
(** Use "git-log" to find the oldest commit with these package versions.
    This avoids invalidating the Docker build cache on every update to opam-repository. *)
