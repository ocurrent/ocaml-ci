module Client = Ocaml_ci_api.Client

let ( >>!= ) = Git_forge.( >>!= )
let render = View.Index.render

let list_orgs prefix ci =
  let open Lwt.Infix in
  Backend.ci ci >>= Client.CI.orgs_detailed >>!= fun orgs ->
  let orgs = List.filter (fun o -> o.Client.CI.number_repos > 0) orgs in
  Dream.respond @@ View.Index.list_orgs prefix orgs

let list_all_orgs ~github ~gitlab =
  let open Lwt.Infix in
  Backend.ci gitlab >>= Client.CI.orgs_detailed >>!= fun gitlab_orgs ->
  Backend.ci github >>= Client.CI.orgs_detailed >>!= fun github_orgs ->
  let github_orgs =
    List.filter (fun o -> o.Client.CI.number_repos > 0) github_orgs
  in
  let gitlab_orgs =
    List.filter (fun o -> o.Client.CI.number_repos > 0) gitlab_orgs
  in
  Dream.respond @@ View.Index.list_all_orgs ~github_orgs ~gitlab_orgs
