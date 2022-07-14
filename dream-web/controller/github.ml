open Lwt.Infix

module Client = Ocaml_ci_api.Client
module Capability = Capnp_rpc_lwt.Capability

let not_found = Dream.respond ~status:`Not_Found View.Client_error.not_found

let (>>!=) x f =
  x >>= function
  | Error `Capnp ex -> Dream.respond ~status:`Internal_Server_Error (Fmt.to_to_string Capnp_rpc.Error.pp ex)
  | Ok y -> f y

let list_orgs ci =
  Client.CI.orgs ci >>!= fun orgs ->
    Dream.respond @@ View.Github.list_orgs orgs

let list_repos ~owner org =
  Client.Org.repos org >>!= fun repos ->
    Dream.respond @@ View.Github.list_repos ~owner repos

let handle ~path ci =
  match path with
  | [] -> Dream.log "Listing orgs"; list_orgs ci
  | [owner] -> Dream.log "Listing repos for org: %s" owner; Capability.with_ref (Client.CI.org ci owner) @@ list_repos ~owner
  | [org; repo] -> Dream.log "Not implemented yet %s %s" org repo; not_found
  | _ -> Dream.log "Not supported"; not_found