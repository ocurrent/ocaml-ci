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

let user_guide =
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
              h1 [ txt "Documentation" ];
              p
                [
                  txt
                    "This is a Work In Progress. Thank you for your patience \
                     as we update it with detailed documentation covering each \
                     of the pages of Ocaml-ci.";
                ];
              div ~a:[ a_id "overview" ] [ h2 [ txt "Overview" ] ];
              p
                [
                  txt
                    "OCaml-CI is a continuous integration (CI) tool for OCaml \
                     projects. This development practice encourages developers \
                     to regularly integrate code into a shared repository. \
                     Commits are verified by an automated build, allowing \
                     teams to detect and fix problems early.";
                ];
              p
                [
                  txt "OCaml-CI uses metadata from the project’s ";
                  code [ txt "opam" ];
                  txt " and ";
                  code [ txt "dune" ];
                  txt
                    " files to work out what to build, and it also uses \
                     caching to make builds fast. It takes the information in \
                     the project's opam files to automatically test against \
                     multiple OCaml versions and OS platforms.";
                ];
              p
                [
                  txt
                    "Simply put, OCaml-CI just tells you if a project is \
                     compatible with OCaml 5 or not.";
                ];
              p
                [
                  txt "In ";
                  a
                    ~a:[ a_href "https://ci.ocamllabs.io/getting-started" ]
                    [ txt "Getting Started" ];
                  txt
                    ", you installed the the OCaml-CI app, which follows this \
                     pattern when deployed:";
                ];
              ul
                [
                  li
                    [
                      txt
                        "First, it retreives the list of organisations that \
                         have installed it.";
                    ];
                  li
                    [
                      txt
                        "For each organisation, it gets the list of \
                         repositories to check.";
                    ];
                  li
                    [
                      txt
                        "For each repository, it gets the branches and PRs to \
                         check.";
                    ];
                  li
                    [
                      txt
                        "For each target, it fetches the head commit, \
                         generates a Dockerfile, and builds it.";
                    ];
                ];
              p
                [
                  txt "The generated Dockerfile first adds all the ";
                  code [ txt "*.opam" ];
                  txt
                    " files found in the project, then uses opam to install \
                     all the dependencies, and finally adds the rest of the \
                     source files. This means that rebuilds are often very \
                     fast, because Docker will reuse the previously cached \
                     build step as long as the opam files don’t change.";
                ];
              p
                [
                  txt
                    "A key point is that OCaml-CI will run an opam solve for \
                     all of the dependencies in all the";
                  code [ txt "*.opam" ];
                  txt " files together.";
                ];
              p
                [
                  txt
                    "For example, given a project with the following opam files";
                ];
              pre
                [
                  code
                    [
                      txt "\n bondi.opam\n bondi-lwt.opam\n bondi-async.opam\n ";
                    ];
                ];
              p
                [
                  txt
                    "OCaml-CI will run an opam solve for all of the \
                     dependencies in the three opam files. So, if you depend \
                     on OCaml 5 in ";
                  code [ txt "bondi-lwt.opam," ];
                  txt
                    " then a build will only happen on that version of OCaml, \
                     even if the other two packages could run on earlier \
                     versions of OCaml.";
                ];
              div ~a:[ a_id "concepts" ] [ h2 [ txt "Concepts & Terms" ] ];
              p
                [
                  txt
                    "The following terms are used across OCaml-CI, so it's \
                     best familiarise yourself with them before reading \
                     through this documentation.";
                ];
              p
                [
                  strong [ txt "Build" ];
                  txt
                    " : A collection of steps that correspond to the complete \
                     set of actions taken by OCaml-CI when it is run against a \
                     project. When you start with the build status of a PR, \
                     and click on the build status or OCaml-CI links (e.g., in \
                     GitHub actions), you arrive at a build page.";
                ];
              p
                [
                  strong [ txt "Organisation" ];
                  txt
                    " : An organisation that owns projects that they want to \
                     build. This typically corresponds to an account on GitHub \
                     or GitLab for example.";
                ];
              p
                [
                  strong [ txt "Pipeline" ];
                  txt
                    " : An automated series of actions (steps) necessary for a \
                     build.";
                ];
              p
                [
                  strong [ txt "Project" ];
                  txt " : Used loosely to describe software written in OCaml.";
                ];
              p
                [
                  strong [ txt "Repository" ];
                  txt " : The Git repository that houses the project.";
                ];
              p
                [
                  strong [ txt "Ref" ];
                  txt
                    " : Git branches that exist within the repository. There \
                     is thus the concept of a default `ref` (typically main or \
                     master) and those that correspond to pull requests or \
                     merge requests.";
                ];
              p
                [
                  strong [ txt "Step" ];
                  txt
                    " : A step or a job is a unit of work. For example, \
                     building a project on \
                     platform-x-with-compiler-version-y-with-opam-version-z or \
                     linting a project via OCamlFormat.";
                ];
            ];
          div
            ~a:[ a_class [ "flex flex-col space-y-2" ] ]
            [
              div ~a:[ a_class [ "font-semibold" ] ] [ txt "On this page" ];
              a
                ~a:[ a_href "#overview"; a_class [ "pl-6 text-sm link-hover" ] ]
                [ txt "Overview" ];
              a
                ~a:[ a_href "#concepts"; a_class [ "pl-6 text-sm link-hover" ] ]
                [ txt "Concepts & Terms" ];
            ];
        ];
    ]
