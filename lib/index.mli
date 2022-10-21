(** The index is:

    - A map from active Git references to the Git commit at their heads.
    - A map from project builds ([owner * name * hash)] triples) to statuses.
    - A (persisted) map from each Git commit hash to its last known OCurrent job
      ID. *)

type job_state =
  [ `Not_started | `Active | `Failed of string | `Passed | `Aborted ]
[@@deriving show]

type build_status = [ `Not_started | `Pending | `Failed | `Passed ]

type ref_info = {
  hash : string;
  message : string;
  gref : string;
  started_at : float option;
}

val init : unit -> unit
(** Ensure the database is initialised (for unit-tests). *)

val record :
  repo:Repo_id.t ->
  hash:string ->
  status:build_status ->
  message:string ->
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
val get_message : owner:string -> name:string -> hash:string -> string

val get_build_history :
  owner:string -> name:string -> gref:string -> ref_info list
(** [get_build_history ~owner ~name ~gref] is a list of builds for the branch
    gref of the repo identfied by (owner, name). The builds are identified by
    ref_info *)

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

val set_description : owner:string -> string -> unit
(** [set_description ~owner description] records that [owner] has a
    [description] *)

val get_description : owner:string -> string
(** [get_description ~owner] is the last value passed to [set_description] for
    [owner], or ["Placeholder description"] if not known yet. *)

val set_n_repos : owner:string -> int -> unit
(** [set_n_repos ~owner n_repos] records that [owner] has [n_repos] active
    repositories *)

val get_n_repos : owner:string -> int
(** [get_n_repos ~owner] is the last value passed to [set_n_repos] for [owner],
    or [0] if not known yet. *)

val set_active_repos : owner:string -> Repo_set.t -> unit
(** [set_active_repos ~owner repos] records that [repos] is the set of active
    repositories under [owner]. *)

val get_active_repos : owner:string -> Repo_set.t
(** [get_active_repos ~owner] is the last value passed to [set_active_repos] for
    [owner], or [empty] if not known yet. *)

module Ref_map : Map.S with type key = string

val set_active_refs : repo:Repo_id.t -> string Ref_map.t -> unit
(** [set_active_refs ~repo refs] records that [refs] is the current set of Git
    references that the CI is watching. There is one entry for each branch and
    PR. Each entry maps the Git reference name to the head commit's hash. *)

val get_active_refs : Repo_id.t -> string Ref_map.t
(** [get_active_refs repo] is the entries last set for [repo] with
    [set_active_refs], or [empty] if this repository isn't known. *)

val get_main_ref : Repo_id.t -> (string * string) option
(** [get_active_ref repo] is the main ref (with hash) in the entries last set
    for [repo] with [set_active_refs], or [None] if this repository isn't known. *)
