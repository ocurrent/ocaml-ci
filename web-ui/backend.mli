type t
(** Handles connections to ocaml-ci-service via Capnp *)

val connect : Uri.t -> Ocaml_ci_api.Client.CI.t Lwt.t
