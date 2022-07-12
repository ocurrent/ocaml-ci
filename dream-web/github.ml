open Lwt.Infix
module Client = Ocaml_ci_api.Client

let not_found =
  Dream.respond ~status:`Not_Found ""

let (>>!=) x f =
  x >>= function
  | Error `Capnp ex -> Dream.respond ~status:`Internal_Server_Error (Fmt.to_to_string Capnp_rpc.Error.pp ex)
  | Ok y -> f y

let list_orgs ci =
  Client.CI.orgs ci >>!= fun orgs ->
    Dream.respond @@ View.Github.list_orgs orgs

let handle ~path ci =
  match path with
  | [] -> list_orgs ci
  | _ -> not_found
