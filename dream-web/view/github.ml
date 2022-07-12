let org_url owner =
  Printf.sprintf "/github/%s" owner

let format_org org =
  let open Tyxml.Html in
  li [a ~a:[a_href (org_url org)] [txt org]]

let list_orgs orgs =
  Template.instance Tyxml.Html.[
      (*breadcrumbs [] "github";*)
      ul (List.map format_org orgs)
  ]
