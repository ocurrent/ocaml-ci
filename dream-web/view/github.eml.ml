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

let org_url owner =
  Printf.sprintf "/github/%s" owner

  let repo_url ~owner name =
    Printf.sprintf "/github/%s/%s" owner name

let format_org org =
  let url = org_url org in
  <li><a href=<%s url %>><%s org %></a></li>

let format_repo ~owner { Client.Org.name; master_status } =
  let url = repo_url ~owner name in
  let build_status = Build_status.class_name master_status in
  <li><a class=<%s build_status %> href=<%s url %>><%s name %></a></li>

let org_contents orgs =
  let lis = String.concat "" (List.map format_org orgs) in
  <ul>
      (*breadcrumbs [] "github";*)
    <%s! lis %>
  </ul>

let repo_contents ~owner repos =
  let lis = String.concat "" (List.map (format_repo ~owner) repos) in
  <ul class="statuses">
    <%s! lis %>
  </ul>

let list_orgs orgs =
  Template.instance @@ org_contents orgs

let list_repos ~owner repos =
  Template.instance @@ repo_contents ~owner repos