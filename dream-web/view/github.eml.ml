let org_url owner =
  Printf.sprintf "/github/%s" owner

let format_org org =
  let url = org_url org in
  <li><a href=<%s url %>><%s org %></a></li>

let contents orgs =
  let lis = String.concat "" (List.map format_org orgs) in
  <ul>
      (*breadcrumbs [] "github";*)
    <%s! lis %>
  </ul>

let list_orgs orgs =
  Template.instance @@ contents orgs
