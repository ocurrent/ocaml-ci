open Tyxml.Html

let show =
  [
    h2 ~a:[ a_id "history-page" ] [ txt "The History Page" ];
    p
      [
        txt
          "The history page shows the build history of a ref. It shows every \
           commit of that branch built by OCaml-CI. Each row that appears on \
           this page corresponds to the latest build of a commit, and clicking \
           on a row takes you to the build page of that commit. Each row \
           identifies the commit, its build status, the time at which the \
           build started, and the running time of the build.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/history-page-overview.png" ~alt:"history-page-overview" ();
  ]
