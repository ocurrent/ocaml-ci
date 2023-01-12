open Tyxml.Html

let show =
  [
    h2 ~a:[ a_id "build-page" ] [ txt "The Build Page" ];
    p
      [
        txt
          "The build page is typically the most important page of a CI. It \
           shows the results of\n\
          \          running the CI on a commit and thus helps determine the \
           repository's health following the additional changes introduced by \
           the commit. It shows the overall status as well as an overview of \
           the steps that constitute the build. When examining a commit on \
           GitHub or GitLab, clicking on the commit's build status will bring \
           you to this page.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/build-page-overview.png" ~alt:"build-page-overview" ();
    h3 [ txt "Salient information regarding a build" ];
    ol
      [
        li [ txt "The status of the build" ];
        li [ txt "The commit that the build corresponds to" ];
        li [ txt "The date and time that the build was created" ];
        li [ txt "The total build run time" ];
        li [ txt "The branch (or branches) that the commit belongs to" ];
        li [ txt "The 'wall clock' run time of the build" ];
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/build-page-top-matter.png" ~alt:"build-page-top-matter" ();
    h3 [ txt "Total build run time" ];
    p
      [
        txt
          "The total build run time is the sum of the running times of the \
           build's steps. It measures the resources engaged during the \
           execution of the build, but it does not take any parallelism into \
           account.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/build-page-total-run-time.png"
      ~alt:"build-page-total-run-time" ();
    h3 [ txt "Build history button" ];
    p [ txt "To see the history of builds for the ref, click this button." ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/build-page-history-button.png"
      ~alt:"build-page-history-button" ();
    h3 [ txt "Running time for a build" ];
    p
      [
        txt
          "A build's running time is defined to be the sum of the time taken \
           by the analysis step and that of the longest running step of the \
           build. It represents the 'wall clock' or elapsed time of the build \
           and is different from the 'total build run time,' as it represents \
           the time taken if all steps ran in parallel.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/build-page-running-time.png" ~alt:"build-page-running-time"
      ();
    h3 [ txt "Cancelling a running build" ];
    p
      [
        txt
          "The 'Cancel' button allows the cancellation of a build that is \
           running or that has not yet started.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/build-page-cancel.png" ~alt:"build-page-cancel" ();
    h3 [ txt "Rebuilding" ];
    p [ txt "A build that has stopped or finished can be rebuilt." ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/build-page-rebuild.png" ~alt:"build-page-rebuild" ();
    p
      [
        txt
          "There are two options available on clicking the 'Rebuild' button. \
           'Rebuild all' rebuilds all steps (except for analysis). 'Rebuild \
           failed' only rebuilds the failing steps.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/build-page-rebuild-all-failed.png"
      ~alt:"build-page-rebuild-all-failed" ();
    h3 [ txt "Steps" ];
    p
      [
        txt
          "The table of steps shown on the build page provides summary \
           information of each build step. Each table row corresponds to a \
           step; it shows the name of the step, its build status, the date and \
           time created, the time spent in an enqueued state, and its running \
           time.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/build-page-step-rows.png" ~alt:"build-page-step-rows" ();
    p
      [
        txt
          "To see the details of a step, including its logs, click on the row.";
      ];
  ]
