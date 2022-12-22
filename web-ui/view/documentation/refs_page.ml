open Tyxml.Html

let show =
  [
    h2 ~a:[ a_id "refs-page" ] [ txt "The Refs Page" ];
    p
      [
        txt
          "The refs page (of a repository) shows all git branches of that \
           repository that have been built on OCaml-CI. Each row corresponds \
           to a ref, showing information about its latest build. Refs are \
           separated into three groups - the default branch of the repository, \
           branches that do not correspond to pull or merge requests, and \
           branches that do.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/refs-page-overview.png" ~alt:"refs-overview" ();
    h3 [ txt "Salient information regarding each ref" ];
    p
      [
        txt
          "Each ref is shown with a snapshot of information relevant to its \
           latest build";
      ];
    ol
      [
        li [ txt "The status of the latest build" ];
        li [ txt "The name of the ref" ];
        li [ txt "The message corresponding to the latest commit" ];
        li [ txt "The latest commit" ];
        li [ txt "The pull or merge request that the ref corresponds to" ];
        li [ txt "The date and time of the latest build" ];
        li [ txt "The running time of the latest build" ];
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/refs-page-row.png" ~alt:"refs-row" ();
    p [ txt "To see the latest build of a ref, click on the row." ];
  ]
