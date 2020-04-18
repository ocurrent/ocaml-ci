open Tyxml.Html

let render () =
  Template.instance [
    p [txt "Welcome to Opam-CI!"];
    p [txt "See ";
       a ~a:[a_href "https://github.com/apps/opam-ci"] [
         txt "The Opam-CI GitHub App"
       ];
       txt " for details.";
      ];
    ul [
      li [a ~a:[a_href "/github"] [txt "Registered GitHub organisations"]];
    ]
  ]
