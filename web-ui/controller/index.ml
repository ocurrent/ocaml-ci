module Client = Ocaml_ci_api.Client

let ( >>!!= ) = Git_forge.( >>!= )
let ( >|!= ) a f = Lwt_result.map f a

let list_orgs ~orgs =
  let open Lwt.Infix in
  let extract ((prefix, name, backend) : string * string * Backend.t) =
    Backend.ci backend
    >>= Client.CI.orgs_detailed
    >|!= List.filter (fun o -> o.Client.CI.number_repos > 0)
    >|!= fun orgs -> (prefix, name, orgs)
  in
  let to_result_list orgs org =
    match org with
    | Ok org -> Result.map (fun orgs -> org :: orgs) orgs
    | Error _ -> orgs
  in
  Lwt_list.map_p extract orgs
  |> Lwt.map (fun orgs -> List.fold_left to_result_list (Ok []) orgs)
  >>!!= fun orgs -> Dream.respond @@ View.Index.list_orgs ~orgs
