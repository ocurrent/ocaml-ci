open Tyxml.Html

let show =
  [
    div ~a:[ a_id "index-page" ] [ h2 [ txt "The Index Page" ] ];
    div
      [
        txt "After ";
        a
          ~a:[ a_href "https://ci.ocamllabs.io/getting-started" ]
          [ txt "installing OCaml-CI," ];
        txt
          " the service will register your organisation and build the \
           repositories that were nominated during the installation process.";
      ];
    p
      [
        txt
          "The index page of OCaml-CI lists all organisations that OCaml-CI \
           knows about, across both git forges, GitHub and GitLab. The logo of \
           the git forge is used to indicate the source of the organisation.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/index-page-overview.png" ~alt:"index-page-overview" ();
    p
      [
        txt
          "A filter is provided to select between GitHub or GitLab. Search \
           functionality is available to locate the organisation of interest.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/index-page-search.png" ~alt:"index-page-search" ();
    p [ txt "To see its repositories, click on the organisation." ];
  ]
