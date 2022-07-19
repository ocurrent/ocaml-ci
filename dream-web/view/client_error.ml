open Tyxml.Html

let not_found =
  Template_tyxml.instance [
    p [txt "Not Found"];
  ]

let internal_server_error =
  Template_tyxml.instance [
    p [txt "Internal Server Error"]
  ]
