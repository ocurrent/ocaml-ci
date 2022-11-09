open Tyxml.Html

let getting_started =
  Template_v1.instance
    [
      div
        ~a:[ a_class [ "grid grid-cols-6" ] ]
        [
          div
            ~a:[ a_class [ "blog-sidebar" ] ]
            [
              div
                ~a:[ Tyxml_helpers.x_data "{submenu: false}" ]
                [ (* this is where we would have the sidebar links if we had any *) ];
            ];
          div
            ~a:[ a_class [ "prose col-span-4" ] ]
            [
              h1 [ txt "Getting Started" ];
              p [ txt "OCaml-CI is a CI service for OCaml projects." ];
              hr ();
              p
                [
                  txt
                    "It uses metadata from the project's opam and dune files \
                     to work out what to build, and uses caching to make \
                     builds fast. It uses the information in the opam files in \
                     the project to automatically test against multiple OCaml \
                     versions and OS platforms.";
                ];
              p [ txt "To start building with OCaml-CI:" ];
              ul
                [
                  li
                    [
                      txt "Add the app to your account, selecting ";
                      strong [ txt "Install" ];
                      txt " at ";
                      a
                        ~a:[ a_href "https://github.com/apps/ocaml-ci" ]
                        [ txt "https://github.com/apps/ocaml-ci" ];
                    ];
                  li
                    [
                      txt
                        "Select only the repositories you want tested - if you \
                         select ";
                      strong [ txt "All Repositories" ];
                      txt " we won't build anything!";
                    ];
                ];
              p
                [
                  txt
                    "This service is somewhat experimental and being heavily \
                     worked on under ";
                  a
                    ~a:[ a_href "https://github.com/ocurrent" ]
                    [ txt "https://github.com/ocurrent" ];
                  txt
                    ". You will need to be approved before it will start \
                     building anything. e.g. ";
                  a
                    ~a:
                      [ a_href "https://github.com/ocurrent/ocaml-ci/pull/346" ]
                    [ txt "https://github.com/ocurrent/ocaml-ci/pull/346" ];
                ];
              ul
                [
                  li
                    [
                      txt "Github documentation on how to install ";
                      a
                        ~a:
                          [
                            a_href
                              "https://docs.github.com/en/developers/apps/managing-github-apps/installing-github-apps#installing-your-private-github-app-on-your-repository";
                          ]
                        [ txt "an app on your repository" ];
                    ];
                  li
                    [
                      txt "The source code is at ";
                      a
                        ~a:[ a_href "https://github.com/ocurrent/ocaml-ci" ]
                        [ txt "https://github.com/ocurrent/ocaml-ci" ];
                    ];
                ];
            ];
        ];
    ]
