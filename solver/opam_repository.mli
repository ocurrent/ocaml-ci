val open_store : unit -> Git_unix.Store.t Lwt.t

val clone : unit -> unit Lwt.t
(** [clone ()] ensures that "./opam-repository" exists. If not, it clones it. *)

val oldest_commit_with :
  from:Git_unix.Store.Hash.t -> OpamPackage.t list -> string Lwt.t
(** Use "git-log" to find the oldest commit with these package versions. This
    avoids invalidating the Docker build cache on every update to
    opam-repository.

    @param from The commit at which to begin the search. *)

val fetch : unit -> unit Lwt.t
(* Does a "git fetch origin" to update the store. *)
