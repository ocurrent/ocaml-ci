(** Handles connections to ocaml-ci-service via Capnp *)

open Capnp_rpc_lwt

type t

val make : Ocaml_ci_api.Raw.Client.CI.t Sturdy_ref.t -> t
val ci : t -> Ocaml_ci_api.Client.CI.t Lwt.t

module Make : sig
  val ci : Uri.t -> Ocaml_ci_api.Client.CI.t Lwt.t
end
