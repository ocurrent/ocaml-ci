open Lwt.Infix
module Client = Ocaml_ci_api.Client
module Capability = Capnp_rpc_lwt.Capability

let not_found = Dream.respond ~status:`Not_Found View.Client_error.not_found

let ( >>!= ) x f =
  x >>= function
  | Error (`Capnp ex) ->
      Dream.respond ~status:`Internal_Server_Error
        (Fmt.to_to_string Capnp_rpc.Error.pp ex)
  | Ok y -> f y

let list_orgs ci =
  Client.CI.orgs ci >>!= fun orgs ->
  Dream.respond @@ View.Github.list_orgs ~orgs

let list_repos org ci =
  let list_repos' org o =
    Client.Org.repos o >>!= fun repos ->
    Dream.respond @@ View.Github.list_repos ~org ~repos
  in
  Capability.with_ref (Client.CI.org ci org) @@ list_repos' org

let list_refs org name ci =
  let list_refs' org name r =
    Client.Repo.refs r >>!= fun refs ->
    Dream.respond @@ View.Github.list_refs ~org ~name ~refs
  in
  Capability.with_ref (Client.CI.org ci org) @@ fun org' ->
  Capability.with_ref (Client.Org.repo org' name) @@ fun repo ->
  list_refs' org name repo

let get_handler ~path ci =
  match path with
  | [] -> list_orgs ci
  | [ org ] -> list_repos org ci
  | [ org; name ] -> list_refs org name ci
  | [ org; name; path ] ->
      Dream.log "Not implemented yet %s %s %s" org name path;
      not_found
  | _ ->
      Dream.log "Not supported";
      not_found

let post_handler ~path _ =
  Dream.log "POST handlers not yet implemented. Path: %s"
    (String.concat "/" path);
  not_found
