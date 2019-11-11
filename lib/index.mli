(** The index is:
    - A map from active Git references to the Git commit at their heads.
    - A (persisted) map from each Git commit hash to its last known OCurrent job ID. *)

type job_state = [`Not_started | `Active | `Failed of string | `Passed | `Aborted ] [@@deriving show]

val record : repo:Current_github.Repo_id.t -> hash:string -> (string * Current.job_id option) list -> unit
(** [record ~repo ~hash jobs] updates the entry for [repo, hash] to point at [jobs]. *)

val is_known_owner : string -> bool
(** [is_known_owner owner] is [true] iff there is an entry for a commit in organisation [owner]. *)

val is_known_repo : owner:string -> name:string -> bool
(** [is_known_repo ~owner ~name] is [true] iff there is an entry for a commit in repository [owner/name]. *)

val list_owners : unit -> string list
(** [list_owners ()] lists all the tracked owners. *)

val list_repos : string -> string list
(** [list_repos owner] lists all the tracked repos under [owner]. *)

val get_jobs : owner:string -> name:string -> string -> (string * job_state) list
(** [get_jobs ~owner ~name commit] is the last known set of OCurrent jobs for hash [commit] in repository [owner/name]. *)

val get_job : owner:string -> name:string -> hash:string -> variant:string -> (string option, [> `No_such_variant]) result
(** [get_job ~owner ~name ~variant] is the last known job ID for this combination. *)

val get_full_hash : owner:string -> name:string -> string -> (string, [> `Ambiguous | `Unknown | `Invalid]) result
(** [get_full_hash ~owner ~name short_hash] returns the full hash for [short_hash]. *)

module Repo_map : Map.S with type key = Current_github.Repo_id.t

val set_active_refs : repo:Current_github.Repo_id.t -> (string * string) list -> unit
(** [set_active_refs ~repo entries] records that [entries] is the current set of Git references that the CI
    is watching. There is one entry for each branch and PR. Each entry is a pair of the Git reference name
    and the head commit's hash. *)

val get_active_refs : Current_github.Repo_id.t -> (string * string) list
(** [get_active_refs repo] is the entries last set for [repo] with [set_active_refs], or
    [] if this repository isn't known. *)
