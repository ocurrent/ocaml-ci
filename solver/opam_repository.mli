val open_store : ?repo_url:string -> unit -> Git_unix.Store.t Lwt.t
(** Open the local clone of the repo at the given URL.
    If the local clone does not yet exist, this clones it first.
    If repo_url is unspecified, it defaults to ocaml/opam-repository on GitHub. *)

val clone : ?repo_url:string -> unit -> unit Lwt.t
(** [clone ()] ensures that a local clone of the specified repo exists. If not, it clones it.
    If repo_url is unspecified, it defaults to ocaml/opam-repository on GitHub. *)

val oldest_commits_with : from:(string * string) list -> OpamPackage.t list -> (string * string) list Lwt.t
(** Use "git-log" to find the oldest commits with these package versions.
    This avoids invalidating the Docker build cache on every update to opam-repository.
    @param from The repo_url and commit hash for each opam_repository at which to
                begin the search. *)

val fetch : ?repo_url:string -> unit -> unit Lwt.t
(* Does a "git fetch origin" to update the store. *)
