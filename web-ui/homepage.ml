open Tyxml.Html

let render () =
  Template.instance [
    p [txt "Welcome to OCaml-CI for GitLab!"];
    ul [
      li [a ~a:[a_href "/gitlab"] [txt "Registered GitLab organisations"]];
    ]
  ]
