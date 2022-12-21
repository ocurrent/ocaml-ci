open Tyxml.Html

let show =
  [
    div ~a:[ a_id "step-page" ] [ h2 [ txt "Step page walkthrough" ] ];
    p
      [
        txt
          "The step page shows the results of running a step on OCaml-CI. If a \
           step has failed, typically the step's page will help understand and \
           diagnose the cause of the failure. The logs displayed on this page \
           contain details of exactly what OCaml-CI did to execute the step \
           and will contain information such as stack traces and/or error \
           messages that will help identify the issue.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/step-page-overview.png" ~alt:"step-page-overview" ();
    div [ h3 [ txt "Salient information regarding the step" ] ];
    p [ txt "The step page has the following key pieces of information:" ];
    ol
      [
        li
          [
            txt
              "The status of the build is indicated as a green tick for \
               Passed, a red cross for Failed, a yellow circle for Running and \
               a grey circle for Unknown";
          ];
        li
          [
            txt
              "The title describes the step - e.g. \
               platform-x-with-compiler-version-y-with-opam-version-z or \
               linting a project";
          ];
        li
          [
            txt
              "There are timestamps to indicate when the step was created and \
               when it finished";
          ];
        li
          [
            txt
              "The amount of time that the step was enqueued (before it \
               started running) appears between these timestamps";
          ];
        li
          [
            txt
              "The specific commit that was built. This is hyperlinked to the \
               commit on the relevant git-forge.";
          ];
        li
          [
            txt
              "The run time of the step appears on the right of the top matter \
               and refers to the total time that the step ran for.";
          ];
      ];
    div [ h3 [ txt "The logs" ] ];
    p
      [
        txt
          "Logs are generated during the build process. To refer to a specific \
           portion of the logs, click on the first line of interest and \
           holding the Shift key, click on the last line of interest. This \
           will result in a block of lines being highlighted. To copy a URL to \
           the step page that will highlight exactly these lines, click on the \
           button that appears in the bottom left of the highlighted block.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/step-page-logs-highlighted.png"
      ~alt:"step-page-logs-highlighted" ();
    div [ h3 [ txt "Rebuilding and cancelling a step" ] ];
    p
      [
        txt
          "When a step is running it can be cancelled by clicking on the \
           Cancel button.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/step-page-cancel.png" ~alt:"step-page-cancel" ();
    p
      [
        txt
          "Similarly, when a step has finished, it can be rebuilt by clicking \
           on the Rebuild button.";
      ];
    img
      ~a:[ a_class [ "border border-solid" ] ]
      ~src:"/images/step-page-rebuild.png" ~alt:"step-page-rebuild" ();
  ]