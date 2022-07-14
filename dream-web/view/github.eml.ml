module Client = Ocaml_ci_api.Client

module Build_status = struct
  include Client.Build_status

  let class_name (t : t) =
    match t with
    | NotStarted -> "not-started"
    | Failed -> "failed"
    | Passed -> "passed"
    | Pending -> "active"
    | Undefined _ -> "undefined"
end

let org_url org =
  Printf.sprintf "/github/%s" org

let repo_url org name =
  Printf.sprintf "/github/%s/%s" org name

let format_org org =
  let url = org_url org in
  <li>
    <a href=<%s url %>><%s org %></a>
  </li>

let commit_url ~org ~name hash =
  Printf.sprintf "/github/%s/%s/commit/%s" org name hash

let format_repo ~org { Client.Org.name; master_status } =
  let url = repo_url org name in
  let build_status = Build_status.class_name master_status in
  <li class=<%s build_status %>>
    <a href=<%s url %>><%s name %></a>
  </li>

let orgs_v ~orgs =
  let lis = String.concat "" (List.map format_org orgs) in
  <ul>
    <%s! lis %>
  </ul>

let repos_v ~org ~repos =
  let lis = String.concat "" (List.map (format_repo ~org) repos) in
  <ul class="statuses">
    <%s! lis %>
  </ul>

let format_ref ~org ~name ~branch ~commit ~status =
  let url = commit_url ~org ~name commit in
  let build_status = Build_status.class_name status in 
  <li class=<%s build_status %>>
    <a href=<%s url %>><%s branch %></a>
  </li>

let refs_v ~org ~name ~refs =
  let lis = 
    let lis' = Client.Ref_map.bindings refs |> List.map @@ fun (branch, (commit, status)) ->
      format_ref ~org ~name ~branch ~commit ~status
    in
      String.concat "" lis'
  in 
  <ul class="statuses">
    <%s! lis %>
  </ul>  

let list_orgs ~orgs =
  Template.instance @@ orgs_v ~orgs

let list_repos ~org ~repos =
  Template.instance @@ repos_v ~org ~repos

let list_refs ~org ~name ~refs = 
  Template.instance @@ refs_v ~org ~name ~refs