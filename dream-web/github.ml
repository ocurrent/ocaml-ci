open Lwt.Infix
module Client = Ocaml_ci_api.Client

let (>>!=) x f =
  x >>= function
  | Error `Capnp ex -> Dream.respond ~status:`Internal_Server_Error (Fmt.to_to_string Capnp_rpc.Error.pp ex)
  | Ok y -> f y


let org_url owner =
  Printf.sprintf "/github/%s" owner

let format_org org =
  let open Tyxml.Html in
  li [a ~a:[a_href (org_url org)] [txt org]]

let list_orgs ci =
  Client.CI.orgs ci >>!= fun orgs ->
    Dream.respond @@ Template.instance Tyxml.Html.[
          (*breadcrumbs [] "github";*)
      ul (List.map format_org orgs)
    ]
