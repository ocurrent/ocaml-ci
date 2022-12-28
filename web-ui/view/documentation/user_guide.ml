open Tyxml.Html
module Template = View.Template
module Tyxml_helpers = View.Tyxml_helpers

let intro =
  [
    h1 [ txt "Documentation" ];
    p
      [
        txt
          "This is a Work In Progress. Thank you for your patience as we \
           update it with detailed documentation covering each of the OCaml-CI pages.";
      ];
  ]

let overview =
  [
    div ~a:[ a_id "overview" ] [ h2 [ txt "Welcome to OCaml-CI" ] ];
    p
      [
        txt
          "OCaml-CI is a Continuous Integration tool for OCaml projects. It \
           helps the development process by automatically testing code against \
           several versions of the OCaml compiler and various operating \
           systems.";
      ];
    p
      [
        txt
          "Continuous Integration (CI) is an automated testing process used to \
           monitor changes to a central repository of code. A CI performs a \
           series of automated steps (or jobs), e.g., building, testing, and \
           deploying code. Developers can confidently and regularly integrate \
           code into the central repository, relying on the CI system's\n\
          \           automated build to detect and fix problems early.";
      ];
    p
      [
        txt "OCaml-CI uses metadata from the project’s ";
        code [ txt "opam" ];
        txt " and ";
        code [ txt "dune" ];
        txt
          " files to work out what to build, and it also uses caching to make \
           builds fast. It takes the information in the project's opam files \
           to automatically test against multiple OCaml versions and OS \
           platforms.";
      ];
    p
      [
        txt "In ";
        a
          ~a:[ a_href "https://ci.ocamllabs.io/getting-started" ]
          [ txt "Getting Started" ];
        txt
          ", you installed the the OCaml-CI app, which follows this pattern \
           when deployed:";
      ];
    ul
      [
        li
          [
            txt
              "First, it retrieves the list of organisations that have \
               installed it.";
          ];
        li
          [
            txt
              "For each organisation, it gets the list of repositories to \
               check.";
          ];
        li [ txt "For each repository, it gets the branches and PRs to check." ];
        li
          [
            txt
              "For each target, it fetches the head commit, generates a \
               Dockerfile, and builds it.";
          ];
      ];
    p
      [
        txt "The generated Dockerfile first adds all the ";
        code [ txt "*.opam" ];
        txt
          " files found in the project, then uses opam to install all the \
           dependencies, and finally adds the rest of the source files. This \
           means that rebuilds are often very fast, because Docker will reuse \
           the previously cached build step as long as the opam files don’t \
           change.";
      ];
    p
      [
        txt
          "A key point is that OCaml-CI will run an opam solve for all of the \
           dependencies in all the";
        code [ txt "*.opam" ];
        txt " files together.";
      ];
    p [ txt "For example, given a project with the following opam files:" ];
    pre [ code [ txt "\n bondi.opam\n bondi-lwt.opam\n bondi-async.opam\n " ] ];
    p
      [
        txt
          "OCaml-CI will run an opam solve for all of the dependencies in these \
           three opam files. So, if you depend on OCaml 5 in ";
        code [ txt "bondi-lwt.opam," ];
        txt
          " then a build will only happen on that version of OCaml, even if \
           the other two packages could run on earlier versions of OCaml.";
      ];
  ]

let concepts_and_terms =
  [
    div ~a:[ a_id "concepts" ] [ h2 [ txt "Concepts & Terms" ] ];
    p
      [
        txt
          "The following terms are used across OCaml-CI, so it's best \
           familiarise yourself with them before reading through this \
           documentation.";
      ];
    p
      [
        strong [ txt "Git forge" ];
        txt
          " : A service that hosts Git repositories. Currently GitHub and \
           GitLab are supported.";
      ];
    p
      [
        strong [ txt "Project" ];
        txt " : Used loosely to describe software written in OCaml.";
      ];
    p
      [
        strong [ txt "Organisation" ];
        txt
          " : An organisation that owns projects that they want to build. This \
           typically corresponds to an account on a Git forge.";
      ];
    p
      [
        strong [ txt "Repository" ];
        txt " : The Git repository that houses the project.";
      ];
    p
      [
        strong [ txt "Build" ];
        txt
          " : A collection of steps that correspond to the complete set of \
           actions taken by OCaml-CI when it is run against a project. The \
           build status of a branch or pull request usually corresponds to the \
           outcome of the build of the latest commit of that branch or pull \
           request.";
      ];
    p
      [
        strong [ txt "Pipeline" ];
        txt " : An automated series of actions (steps) necessary for a build.";
      ];
    p
      [
        strong [ txt "Ref" ];
        txt
          " : Git branches that exist within the repository. There is thus the \
           concept of a default ref (typically main or master) and those that \
           correspond to pull requests or merge requests.";
      ];
    p
      [
        strong [ txt "Step" ];
        txt
          " : A step or a job is a unit of work. For example, building a \
           project on platform-x-with-compiler-version-y-with-opam-version-z \
           or linting a project via OCamlFormat. The first step of a build is \
           the";
        em [ txt " analysis step " ];
        txt "which plans the rest of the steps of the build.";
      ];
    p
      [
        strong [ txt "Running time" ];
        txt
          " : The running time of a step is the duration for which a step \
           runs. That is, the time elapsed between the point where it starts \
           running and the point where it stops. There are two notions of this \
           concept for a build.";
      ];
    p
      [
        txt "The ";
        strong [ em [ txt "total build run time" ] ];
        txt " is the sum of the running times of the build's steps. The ";
        strong [ em [ txt "build run time" ] ];
        txt
          " is the sum of the time taken by the analysis step and that of the \
           longest running step of the build. It represents the 'wall clock' \
           time of the build";
      ];
  ]

let links =
  div
    ~a:[ a_class [ "flex flex-col space-y-2 pb-6 md:pb-0" ] ]
    [
      div ~a:[ a_class [ "font-semibold" ] ] [ txt "On this page" ];
      a
        ~a:[ a_href "#overview"; a_class [ "pl-6 text-sm link-hover" ] ]
        [ txt "Overview" ];
      a
        ~a:[ a_href "#concepts"; a_class [ "pl-6 text-sm link-hover" ] ]
        [ txt "Concepts & Terms" ];
      a
        ~a:[ a_href "#index-page"; a_class [ "pl-6 text-sm link-hover" ] ]
        [ txt "Index Page" ];
      a
        ~a:[ a_href "#repos-page"; a_class [ "pl-6 text-sm link-hover" ] ]
        [ txt "Repositories Page" ];
      a
        ~a:[ a_href "#refs-page"; a_class [ "pl-6 text-sm link-hover" ] ]
        [ txt "Refs Page" ];
      a
        ~a:[ a_href "#build-page"; a_class [ "pl-6 text-sm link-hover" ] ]
        [ txt "Build Page" ];
      a
        ~a:[ a_href "#step-page"; a_class [ "pl-6 text-sm link-hover" ] ]
        [ txt "Step Page" ];
      a
        ~a:[ a_href "#history-page"; a_class [ "pl-6 text-sm link-hover" ] ]
        [ txt "History Page" ];
    ]

let show =
  let content =
    intro
    @ overview
    @ concepts_and_terms
    @ Index_page.show
    @ Repo_page.show
    @ Refs_page.show
    @ Build_page.show
    @ Step_page.show
    @ History_page.show
  in
  Template.instance
    [
      div
        ~a:
          [
            a_class
              [
                "flex flex-col-reverse md:grid md:grid-cols-6 \
                 dark:text-gray-200";
              ];
          ]
        [
          div
            ~a:[ a_class [ "blog-sidebar" ] ]
            [
              div
                ~a:[ Tyxml_helpers.x_data "{submenu: false}" ]
                [ (* this is where we would have the sidebar links if we had any *) ];
            ];
          div ~a:[ a_class [ "prose dark:prose-invert col-span-4" ] ] content;
          links;
        ];
    ]
