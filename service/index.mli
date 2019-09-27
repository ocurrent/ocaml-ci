(** The index is:
    - A map from active Git references to the Git commit at their heads.
    - A (persisted) map from each Git commit hash to its last known OCurrent job ID. *)

val record : commit:Current_github.Api.Commit.t -> Current.job_id -> unit
(** [record ~commit job_id] updates the entry for [commit] to point at [job_id]. *)

val is_known_owner : string -> bool
(** [is_known_owner owner] is [true] iff there is an entry for a commit in organisation [owner]. *)

val is_known_repo : owner:string -> name:string -> bool
(** [is_known_repo ~owner ~name] is [true] iff there is an entry for a commit in repository [owner/name]. *)

val get_job : owner:string -> name:string -> string -> (Current.job_id, [> `Unknown | `Ambiguous]) result
(** [get_job ~owner ~name commit] is the last known OCurrent job for hash [commit] in repository [owner/name]. *)

module Repo_map : Map.S with type key = Current_github.Repo_id.t

val set_active_refs : repo:Current_github.Repo_id.t -> (string * string) list -> unit
(** [set_active_refs ~repo entries] records that [entries] is the current set of Git references that the CI
    is watching. There is one entry for each branch and PR. Each entry is a pair of the Git reference name
    and the head commit's hash. *)

val get_active_refs : Current_github.Repo_id.t -> (string * string) list
(** [get_active_refs repo] is the entries last set for [repo] with [set_active_refs], or
    [] if this repository isn't known. *)
