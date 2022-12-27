open Tyxml.Html
module Template = View.Template
module Tyxml_helpers = View.Tyxml_helpers

let show =
  Template.instance
    [
      div
        ~a:[ a_class [ "flex flex-col md:grid md:grid-cols-6" ] ]
        [
          div
            ~a:[ a_class [ "blog-sidebar" ] ]
            [
              div
                ~a:[ Tyxml_helpers.x_data "{submenu: false}" ]
                [ (* this is where we would have the sidebar links if we had any *) ];
            ];
          div
            ~a:[ a_class [ "prose dark:prose-invert col-span-4" ] ]
            [
              h1 [ txt "Getting Started" ];
              p [ txt "OCaml-CI is a CI service for OCaml projects." ];
              hr ();
              p
                [
                  txt "OCaml-CI uses metadata from the project's ";
                  code [ txt "opam" ];
                  txt " and ";
                  code [ txt "dune" ];
                  txt
                    " files to work out what to build, and it also uses \
                     caching to make builds fast. It takes the information in \
                     the project's ";
                  code [ txt "opam" ];
                  txt
                    " files to automatically test against multiple OCaml \
                     versions and OS platforms.";
                ];
              p [ txt "To start building with OCaml-CI:" ];
              ul
                [
                  li
                    [
                      txt "Add the app to your account by selecting ";
                      strong [ txt "Configure" ];
                      txt " at ";
                      a
                        ~a:[ a_href "https://github.com/apps/ocaml-ci" ]
                        [ txt "https://github.com/apps/ocaml-ci" ];
                      img ~src:"/images/github-apps-1.png"
                        ~alt:"Click Configure" ();
                    ];
                  li
                    [
                      txt "Select the GitHub account or organisation and click ";
                      strong [ txt "Configure" ];
                      img ~src:"/images/github-apps-2.png"
                        ~alt:"Click Configure" ();
                    ];
                  li
                    [
                      txt
                        "Select only the repositories you want tested \
                         (starting with no more than three please). If you \
                         select ";
                      strong [ txt "All Repositories" ];
                      txt " we won't be able to build anything!";
                      img ~src:"/images/github-apps-3.png"
                        ~alt:"Click Configure" ();
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
                     building anything. This is done by adding yourself to a \
                     list by submitting a PR (e.g. ";
                  a
                    ~a:
                      [ a_href "https://github.com/ocurrent/ocaml-ci/pull/346" ]
                    [ txt "https://github.com/ocurrent/ocaml-ci/pull/346" ];
                  txt ").";
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
