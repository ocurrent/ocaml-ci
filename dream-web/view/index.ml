open Tyxml.Html

let render =
  Template_tyxml.instance [
    p [txt "Welcome to OCaml-CI!"];
    p [txt "See ";
       a ~a:[a_href "https://github.com/apps/ocaml-ci"] [
         txt "The OCaml-CI GitHub App"
       ];
       txt " for details.";
      ];
    ul [
      li [a ~a:[a_href "/github"] [txt "Registered GitHub organisations"]];
    ]
  ]
