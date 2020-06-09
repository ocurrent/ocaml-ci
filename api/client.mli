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
end

type job_info = {
  variant : variant;
  outcome : State.t;
}

module Commit : sig
  type t = Raw.Client.Commit.t Capability.t
  (** A single commit being tested. *)

  val jobs : t -> (job_info list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t

  val job_of_variant : t -> variant -> Current_rpc.Job.t
  (** [job_of_variant t] is the (most recent) OCurrent job for this variant. *)

  val refs : t -> (git_ref list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
  (** [refs t] is the list of Git references that have this commit as their head. *)

  val status : t -> ([ `Not_started | `Pending | `Failed | `Passed ], [> `Capnp of Capnp_rpc.Error.t | `Msg of string]) Lwt_result.t
  (** [status t] is the result of the most-recent 'summarise' step on this commit. *)
end

module Repo : sig
  type t = Raw.Client.Repo.t Capability.t
  (** A GitHub repository that is tested by ocaml-ci. *)

  val refs : t -> ((git_hash * Build_status.t) Ref_map.t, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
  (** [refs t] returns the known Git references (branches and pull requests) that ocaml-ci
      is monitoring, along with the current head of each one. *)

  val commit_of_hash : t -> git_hash -> Commit.t
  (** [commit_of_hash t hash] is the commit [hash] in this repository. *)

  val commit_of_ref : t -> git_ref -> Commit.t
  (** [commit_of_ref t gref] is the commit at the head of Git reference [gref]. *)
end

module Org : sig
  type t = Raw.Client.Org.t Capability.t
  (** A GitHub organisation. *)

  type repo_info = {
    name : string;
    master_status : Build_status.t;
  }

  val repo : t -> string -> Repo.t
  (** [repo t name] is the GitHub organisation at "https://github.com/$owner/$name".
      It returns an error if ocaml-ci doesn't know about this repository. *)

  val repos : t -> (repo_info list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
end

module CI : sig
  type t = Raw.Client.CI.t Capability.t
  (** The top-level object for ocaml-ci. *)

  val org : t -> string -> Org.t
  (** [org t owner] is the GitHub organisation at "https://github.com/$owner".
      It returns an error if ocaml-ci doesn't know about this organisation. *)

  val orgs : t -> (string list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
end
