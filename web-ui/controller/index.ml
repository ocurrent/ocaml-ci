module Client = Ocaml_ci_api.Client

let ( >>!= ) = Git_forge.( >>!= )

let list_orgs ~orgs =
  let open Lwt.Syntax in
  let extract (prefix, name, backend) =
    let* ci = Backend.ci backend in
    let+ detailed_orgs = Client.CI.orgs_detailed ci in
    let filter orgs =
      let orgs = List.filter (fun o -> o.Client.CI.number_repos > 0) orgs in
      (prefix, name, orgs)
    in
    Result.map filter detailed_orgs
  in
  let to_lwt_result acc res =
    Result.bind res (fun res -> Result.map (fun acc -> res :: acc) acc)
  in
  Lwt_list.map_p extract orgs
  |> Lwt.map (fun orgs -> List.fold_left to_lwt_result (Ok []) orgs)
  >>!= fun orgs -> Dream.respond @@ View.Index.list_orgs ~orgs
