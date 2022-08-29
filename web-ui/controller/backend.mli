type t
(** Handles connections to a service via Capnp. *)

val ci : t -> Ocaml_ci_api.Client.CI.t Lwt.t

val make : Uri.t -> t
(** Create connection a service via Capnp.

    Sets up an Lwt monitor to handle reconnections and prometheus metrics. *)
