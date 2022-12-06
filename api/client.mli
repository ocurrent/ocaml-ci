open Capnp_rpc_lwt

type git_ref = string
(** A Git reference. e.g. "refs/heads/master" *)

type git_hash = string
type variant = string

module Ref_map : Map.S with type key = git_ref

module Build_status : sig
  type t = Raw.Reader.BuildStatus.t

  val pp : t Fmt.t
end

module State : sig
  type t = Raw.Reader.JobInfo.State.unnamed_union_t

  val pp : t Fmt.t
  val from_build_status : [< `Failed | `Not_started | `Passed | `Pending ] -> t
end

type job_info = {
  variant : variant;
  outcome : State.t;
  queued_at : float option;
  started_at : float option;
  finished_at : float option;
}

module Commit : sig
  type t = Raw.Client.Commit.t Capability.t
  (** A single commit being tested. *)

  val jobs : t -> (job_info list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t

  val job_of_variant : t -> variant -> Current_rpc.Job.t
  (** [job_of_variant t] is the (most recent) OCurrent job for this variant. *)

  val refs : t -> (git_ref list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
  (** [refs t] is the list of Git references that have this commit as their
      head. *)

  val status :
    t ->
    ( [ `Not_started | `Pending | `Failed | `Passed ],
      [> `Capnp of Capnp_rpc.Error.t | `Msg of string ] )
    Lwt_result.t
  (** [status t] is the result of the most-recent 'summarise' step on this
      commit. *)

  val message :
    t ->
    (variant, [> `Capnp of Capnp_rpc.Error.t | `Msg of string ]) Lwt_result.t
end

module Repo : sig
  type t = Raw.Client.Repo.t Capability.t
  (** A GitHub repository that is tested by ocaml-ci. *)

  type ref_info = {
    gref : string;
    hash : string;
    status : Build_status.t;
    started_at : float option;
    message : string;
    name : string;
    ran_for : float option;
  }

  val refs :
    t -> (ref_info Ref_map.t, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
  (** [refs t] returns the known Git references (branches and pull requests)
      that ocaml-ci is monitoring, along with the current head of each one. *)

  val default_ref :
    t -> (ref_info, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
  (** [default_ref t] returns the default branch of the repo that ocaml-ci is
      monitoring. *)

  val commit_of_hash : t -> git_hash -> Commit.t
  (** [commit_of_hash t hash] is the commit [hash] in this repository. *)

  val commit_of_ref : t -> git_ref -> Commit.t
  (** [commit_of_ref t gref] is the commit at the head of Git reference [gref]. *)

  val history_of_ref :
    t ->
    git_ref ->
    (ref_info list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
  (** [history_of_ref t gref] is the list of builds for the Git reference [gref] *)
end

module Org : sig
  type t = Raw.Client.Org.t Capability.t
  (** A GitHub organisation. *)

  type repo_info = {
    name : string;
    main_status : Build_status.t;
    main_hash : string;
    main_last_updated : float option;
  }

  val repo : t -> string -> Repo.t
  (** [repo t name] is the GitHub organisation at
      "https://github.com/$owner/$name". It returns an error if ocaml-ci doesn't
      know about this repository. *)

  val repos :
    t -> (repo_info list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
end

module CI : sig
  type t = Raw.Client.CI.t Capability.t
  (** The top-level object for ocaml-ci. *)

  val org : t -> string -> Org.t
  (** [org t owner] is the GitHub organisation at "https://github.com/$owner".
      It returns an error if ocaml-ci doesn't know about this organisation. *)

  val orgs : t -> (string list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
end
