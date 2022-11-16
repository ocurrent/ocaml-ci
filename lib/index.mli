(** The index is:

    - A map from active Git references to the Git commit at their heads.
    - A map from project builds ([owner * name * hash)] triples) to statuses.
    - A (persisted) map from each Git commit hash to its last known OCurrent job
      ID. *)

type job_state =
  [ `Not_started | `Active | `Failed of string | `Passed | `Aborted ]
[@@deriving show]

type build_status = [ `Not_started | `Pending | `Failed | `Passed ]

val init : unit -> unit Lwt.t
(** Ensure the database is initialised (for unit-tests). *)

val migrate : unit -> unit Current.t
(** [migrate ()] ensures the database is up-to-date when the project is run. If
    the date changes, it will try to run the migration again. **)

val record :
  repo:Repo_id.t ->
  hash:string ->
  status:build_status ->
  gref:string ->
  (string * Current.job_id option) list ->
  unit
(** [record ~repo ~hash ~gref jobs] updates the entry for [repo, hash] to point
    at [jobs]. *)

val get_jobs :
  owner:string ->
  name:string ->
  string ->
  (string * job_state * Run_time.timestamps option) list
(** [get_jobs ~owner ~name commit] is the last known set of OCurrent jobs for
    hash [commit] in repository [owner/name]. *)

val get_job :
  owner:string ->
  name:string ->
  hash:string ->
  variant:string ->
  (string option, [> `No_such_variant ]) result
(** [get_job ~owner ~name ~variant] is the last known job ID for this
    combination. *)

val get_job_ids : owner:string -> name:string -> hash:string -> string list

val get_build_history :
  owner:string ->
  name:string ->
  gref:string ->
  (string * string * string option) list
(** [get_build_history ~owner ~name ~gref] is a list of builds for the branch
    gref of the repo identfied by (owner, name) The builds are identified by
    triples (variant, hash, Some job_id) *)

val get_build_history_with_time :
  owner:string ->
  name:string ->
  gref:string ->
  (string * string * float option) list
(** [get_build_history_with_time ~owner ~name ~gref] is a list of builds for the
    branch gref of the repo identfied by (owner, name). The builds are
    identified by triples (variant, hash, Some timestamp) *)

val get_status : owner:string -> name:string -> hash:string -> build_status
(** [get_status ~owner ~name ~hash] is the latest status for this combination. *)

val get_full_hash :
  owner:string ->
  name:string ->
  string ->
  (string, [> `Ambiguous | `Unknown | `Invalid ]) result
(** [get_full_hash ~owner ~name short_hash] returns the full hash for
    [short_hash]. *)

module Owner_set : Set.S with type elt = string
module Repo_set : Set.S with type elt = string

val set_active_owners : Owner_set.t -> unit
(** [set_active_owners owners] records that [owners] is the set of accounts on
    which the CI is installed. This is displayed in the CI web interface. *)

val get_active_owners : unit -> Owner_set.t
(** [get_active_owners ()] is the last value passed to [set_active_owners], or
    [\[\]] if not known yet. *)

val set_active_repos : owner:string -> Repo_set.t -> unit
(** [set_active_repos ~owner repos] records that [repos] is the set of active
    repositories under [owner]. *)

val get_active_repos : owner:string -> Repo_set.t
(** [get_active_repos ~owner] is the last value passed to [set_active_repos] for
    [owner], or [empty] if not known yet. *)

module Ref_map : Map.S with type key = string

type ref_info = { hash : string; message : string; name : string }
[@@deriving show]

val set_active_refs : repo:Repo_id.t -> ref_info Ref_map.t -> string -> unit
(** [set_active_refs ~repo refs] records that [refs] is the current set of Git
    references that the CI is watching. There is one entry for each branch and
    PR. Each entry maps the Git reference name to the head commit's hash. *)

val get_active_refs : Repo_id.t -> ref_info Ref_map.t
(** [get_active_refs repo] is the entries last set for [repo] with
    [set_active_refs], or [empty] if this repository isn't known. Elements in
    ref tuple are ([hash], [message], [title]) *)

val get_default_gref : Repo_id.t -> string
(** [get_default_gref repo] is the default gref of the repo last set with
    [set_active_refs] *)
    
type status = [ `Not_started | `Pending | `Failed | `Passed ]

(** Aggregation of state for refs and repos. Refs take their state from their
    head commit, and repos take their state from their main/master branch *)
module Aggregate : sig
  type ref_state
  type repo_state

  val get_ref_status : ref_state -> status
  val get_ref_started_at : ref_state -> float option
  val get_ref_ran_for : ref_state -> float option
  val get_repo_status : repo_state -> status
  val get_repo_started_at : repo_state -> float option

  val set_ref_state :
    repo:Repo_id.t ->
    ref:string ->
    status ->
    float option ->
    float option ->
    unit

  val get_ref_state : repo:Repo_id.t -> ref:string -> ref_state
  val get_repo_state : repo:Repo_id.t -> repo_state
end

module Commit_cache : sig
  type commit_state

  val get_status : commit_state -> status
  val get_started_at : commit_state -> float option
  val get_ran_for : commit_state -> float option

  val add :
    owner:string ->
    name:string ->
    hash:string ->
    status ->
    float option ->
    float option ->
    unit

  val find : owner:string -> name:string -> hash:string -> commit_state
end
