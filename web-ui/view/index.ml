open Tyxml.Html

let render github gitlab =
  Template.instance
    [
      p [ txt "Welcome to OCaml-CI!" ];
      p
        [
          txt "See ";
          a
            ~a:[ a_href "https://github.com/apps/ocaml-ci" ]
            [ txt "The OCaml-CI GitHub App" ];
          txt " for details.";
        ];
      ul
        [
          (if Option.is_some github then
           li
             [
               a
                 ~a:[ a_href "/github" ]
                 [ txt "Registered GitHub organisations" ];
             ]
          else li [ txt "No GitHub organisations" ]);
          (if Option.is_some gitlab then
           li
             [
               a
                 ~a:[ a_href "/gitlab" ]
                 [ txt "Registered GitLab organisations" ];
             ]
          else li [ txt "No GitLab organisations" ]);
        ];
    ]
