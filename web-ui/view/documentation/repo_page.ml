open Tyxml.Html

let show =
  [
    h2 ~a:[ a_id "repos-page" ] [ txt "The Repositories Page" ];
    p
      [
        txt
          "Along with a listing of an organisation's known repositories, this \
           page shows an overview and some key metrics of each repository's \
           default branch.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/repo-page-overview.png" ~alt:"repo-page-overview" ();
    h3 [ txt "Salient information regarding each repository" ];
    p
      [
        txt
          "Each repository is shown with a snapshot of information relevant to \
           the builds of the repository's default branch";
      ];
    ol
      [
        li [ txt "The status of the latest build" ];
        li [ txt "The commit of the latest build" ];
        li [ txt "The date and time of the latest build" ];
        li [ txt "A graph depicting build history" ];
        li [ txt "The average time that a build takes to complete" ];
        li [ txt "The reliability of the build" ];
        li [ txt "The frequency of builds" ];
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/repo-page-chart-metrics.png" ~alt:"repo-chart-metrics" ();
    h3 [ txt "Build history" ];
    p
      [
        txt
          "The chart visualises the last 15 builds that have run on the \
           default branch. The height of each bar reflects the build's running \
           time, and its status is represented by its colour. The tooltip on \
           hover provides more information about the build, and clicking on a \
           bar will take you to the corresponding build page.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/repo-page-chart.png" ~alt:"repo-chart" ();
    h3 [ txt "Average running time" ];
    p
      [
        txt
          "The average running time of all the default branch's completed \
           builds. (See the documentation of the Build Page for a precise \
           definition of running time).";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/repo-page-avg-run-time.png" ~alt:"repo-avg-run-time" ();
    h3 [ txt "Reliability" ];
    p [ txt "The percentage of builds of the default branch that passed." ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/repo-page-reliability.png" ~alt:"repo-reliability" ();
    h3 [ txt "Frequency" ];
    p [ txt "The number of builds of the default branch per week." ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/repo-page-frequency.png" ~alt:"repo-frequency" ();
    p [ txt "To see all the Git branches of the repository, click on the row." ];
  ]
