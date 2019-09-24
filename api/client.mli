open Capnp_rpc_lwt

type git_ref = string
(** A Git reference. e.g. "refs/heads/master" *)

type git_hash = string

module Ref_map : Map.S with type key = git_ref

module Repo : sig
  type t = Raw.Client.Repo.t Capability.t
  (** A GitHub repository that is tested by ocaml-ci. *)

  val refs : t -> (git_hash Ref_map.t, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
  (** [refs t] returns the known Git references (branches and pull requests) that ocaml-ci
      is monitoring, along with the current head of each one. *)

  val job_of_commit : t -> git_hash -> Current_rpc.Job.t
  (** [job_of_commit t hash] is the (most recent) OCurrent job for Git commit [hash]. *)

  val job_of_ref : t -> git_ref -> Current_rpc.Job.t
  (** [job_of_ref t gref] is the job for Git reference [gref]. *)
end

module Org : sig
  type t = Raw.Client.Org.t Capability.t
  (** A GitHub organisation. *)

  val repo : t -> string -> Repo.t
  (** [repo t name] is the GitHub organisation at "https://github.com/$owner/$name".
      It returns an error if ocaml-ci doesn't know about this repository. *)
end

module CI : sig
  type t = Raw.Client.CI.t Capability.t
  (** The top-level object for ocaml-ci. *)

  val org : t -> string -> Org.t
  (** [org t owner] is the GitHub organisation at "https://github.com/$owner".
      It returns an error if ocaml-ci doesn't know about this organisation. *)
end
