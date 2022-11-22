module Client = Ocaml_ci_api.Client

let ( >>!= ) = Git_forge.( >>!= )
let render = View.Index.render

let list_orgs prefix ci =
  let open Lwt.Infix in
  Backend.ci ci >>= Client.CI.orgs >>!= fun orgs ->
  Dream.respond @@ View.Index.list_orgs prefix orgs

let list_all_orgs ~github ~gitlab =
  let open Lwt.Infix in
  Backend.ci gitlab >>= Client.CI.orgs >>!= fun gitlab_orgs ->
  Backend.ci github >>= Client.CI.orgs >>!= fun github_orgs ->
  Dream.respond @@ View.Index.list_all_orgs ~github_orgs ~gitlab_orgs
