module Client = Ocaml_ci_api.Client
module Capability = Capnp_rpc_lwt.Capability

val ( >>!= ) :
  ( 'a,
    [< `Capnp of [< `Cancelled | `Exception of Capnp_rpc.Exception.t ] ] )
  result
  Lwt.t ->
  ('a -> Dream.response Lwt.t) ->
  Dream.response Lwt.t

val job_url : org:string -> repo:string -> hash:string -> string -> string
val list_orgs : Client.CI.t -> Dream.response Lwt.t
val list_repos : org:string -> Client.CI.t -> Dream.response Lwt.t
val list_refs : org:string -> repo:string -> Client.CI.t -> Dream.response Lwt.t

val list_steps :
  org:string ->
  repo:string ->
  hash:string ->
  Dream.request ->
  Client.CI.t ->
  Dream.response Lwt.t

val show_step :
  org:string ->
  repo:string ->
  hash:string ->
  variant:string ->
  Dream.request ->
  Client.CI.t ->
  Dream.response Lwt.t

val rebuild_step :
  org:string ->
  repo:string ->
  hash:string ->
  variant:string ->
  Dream.request ->
  Client.CI.t ->
  Dream.response Lwt.t

val cancel_steps :
  org:string ->
  repo:string ->
  hash:string ->
  Dream.request ->
  Client.CI.t ->
  Dream.response Lwt.t

val rebuild_steps :
  rebuild_failed_only:bool ->
  org:string ->
  repo:string ->
  hash:string ->
  Dream.request ->
  Client.CI.t ->
  Dream.response Lwt.t
