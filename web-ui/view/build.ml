let title_card ~status ~card_title ~hash_link ~ref_links ~first_created_at
    ~ran_for ~buttons =
  let ref_links =
    let initial =
      Tyxml.Html.
        [
          div [ hash_link ];
          div [ txt "-" ];
          div ~a:[ a_id "build-created-at" ] [ txt first_created_at ];
        ]
    in
    List.fold_left
      (fun l ref_link ->
        List.append l Tyxml.Html.[ div [ txt "-" ]; div [ ref_link ] ])
      initial ref_links
  in
  Tyxml.Html.(
    div
      ~a:[ a_class [ "justify-between items-center flex" ] ]
      [
        div
          ~a:[ a_class [ "flex items-center space-x-4" ] ]
          [
            div ~a:[ a_id "build-status" ] [ Common.status_icon status ];
            div
              ~a:[ a_class [ "flex flex-col space-y-1" ] ]
              [
                div
                  ~a:[ a_class [ "flex items-center" ] ]
                  [
                    h1 ~a:[ a_class [ "text-xl" ] ] [ txt card_title ];
                    (* a
                       ~a:
                         [
                           a_class [ "btn btn-secondary btn-sm ml-4" ];
                           a_href "#";
                         ]
                       [ txt "View Build History" ]; *)
                  ];
                div
                  ~a:[ a_class [ "text-gray-500" ] ]
                  [
                    div
                      ~a:[ a_class [ "flex space-x-2 text-sm items-baseline" ] ]
                      ref_links;
                  ];
              ];
          ];
        div
          ~a:[ a_class [ "flex items-center justify-between space-x-4" ] ]
          [
            div
              ~a:[ a_id "build-ran-for"; a_class [ "text-sm" ] ]
              [ txt @@ Fmt.str "%s" ran_for ];
            div
              ~a:
                [
                  a_class [ "relative" ];
                  Tyxml_helpers.x_data "{rebuildMenu: false}";
                ]
              buttons;
          ];
      ])

let tabulate rows =
  Tyxml.Html.(
    div
      ~a:[ a_class [ "container-fluid mt-8 flex flex-col space-y-6" ] ]
      [ div ~a:[ a_class [ "table-container" ] ] rows ])

let step_row ~step_title ~created_at ~queued_for ~ran_for ~status ~step_uri =
  Tyxml.Html.(
    a
      ~a:[ a_class [ "table-row" ]; a_href step_uri ]
      [
        div
          ~a:[ a_class [ "flex items-center space-x-3" ] ]
          [
            Common.status_icon status;
            div
              ~a:[ a_class [ "flex items-center space-x-3" ] ]
              [
                div
                  ~a:[ a_class [ "flex flex-col" ] ]
                  [
                    div
                      ~a:[ a_class [ "text-gray-900 text-sm font-medium" ] ]
                      [ txt step_title ];
                    div
                      ~a:[ a_class [ "flex text-sm space-x-2" ] ]
                      [
                        div [ txt @@ Fmt.str "Created at: %s" created_at ];
                        div [ txt "-" ];
                        div [ txt @@ Fmt.str "%s in queue" queued_for ];
                      ];
                  ];
              ];
          ];
        div
          ~a:
            [
              a_class
                [
                  "flex text-sm font-normal text-gray-500 space-x-8 \
                   items-center";
                ];
            ]
          [
            div [ txt @@ Fmt.str "Ran for %s" ran_for ]; Common.right_arrow_head;
          ];
      ])

let tabulate_steps step_rows =
  Tyxml.Html.(
    div
      ~a:[ a_class [ "container-fluid mt-8 flex flex-col space-y-6" ] ]
      [ div ~a:[ a_class [ "table-container" ] ] step_rows ])

let repo_row ~repo_title ~short_hash ~last_updated ~status ~repo_uri =
  Tyxml.Html.(
    a
      ~a:[ a_class [ "table-row" ]; a_href repo_uri ]
      [
        div
          ~a:[ a_class [ "flex items-center space-x-3" ] ]
          [
            Common.status_icon_build status;
            div
              ~a:[ a_class [ "flex items-center space-x-3" ] ]
              [
                div
                  ~a:[ a_class [ "flex flex-col" ] ]
                  [
                    div
                      ~a:[ a_class [ "text-gray-900 text-sm font-medium" ] ]
                      [ txt repo_title ];
                    div
                      ~a:[ a_class [ "flex text-sm space-x-2" ] ]
                      [
                        div [ txt short_hash ];
                        div [ txt "-" ];
                        div [ txt last_updated ];
                      ];
                  ];
              ];
          ];
      ])

let ref_row ~ref_title ~short_hash ~last_updated ~status ~ref_uri ~message =
  let () = ignore last_updated in
  Tyxml.Html.(
    a
      ~a:[ a_class [ "table-row" ]; a_href ref_uri ]
      [
        div
          ~a:[ a_class [ "flex items-center space-x-3" ] ]
          [
            Common.status_icon_build status;
            div
              ~a:[ a_class [ "flex items-center space-x-3" ] ]
              [
                div
                  ~a:
                    [
                      a_class
                        [
                          "font-medium text-gray-700 text-sm px-2 py-1 border \
                           border-gray-300 rounded-lg";
                        ];
                    ]
                  [ txt ref_title ];
                div
                  ~a:[ a_class [ "flex flex-col" ] ]
                  [
                    div
                      ~a:[ a_class [ "text-gray-900 text-sm font-medium" ] ]
                      [ txt message ];
                    div
                      ~a:[ a_class [ "flex text-sm space-x-2" ] ]
                      [ div [ txt short_hash ] ];
                  ];
              ];
          ];
      ])

let poll =
  Tyxml.Html.script ~a:[]
    (Tyxml.Html.Unsafe.data
       {|
       function poll(api_path, timeout, interval) {
        var endTime = Number(new Date()) + (timeout || 2700000); // 45min timeout
        interval = interval || 10000; // 10s

        const iconSuccess = `
          <div class="icon-status icon-status--success">
              <svg class="h-4 w-4" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="#12B76A">
                  <path fill-rule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clip-rule="evenodd"></path>
              </svg>
          </div>`;

        const iconFailed = `
          <div class="icon-status icon-status--failed">
              <svg xmlns="http://www.w3.org/2000/svg" class="h-3 w-3" viewBox="0 0 20 20" fill="#D92D20">
                  <path fill-rule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clip-rule="evenodd"></path>
              </svg>
          </div>`;

        const iconActive = `
          <div class="icon-status icon-status--active">
            <div></div>
          </div>
        `

        const iconQueued = `
          <div class="icon-status icon-status--default">
            <div></div>
          </div>
        `
        var checkCondition = function (resolve, reject) {
          // If the condition is met, we're done!
          fetch(api_path)
            .then((response) => response.json())
            .then((data) => {
              console.log(data);

              const build_created_at = document.getElementById("build-created-at");
              build_created_at.innerHTML = data["first_created_at"];
              const build_ran_for = document.getElementById("build-ran-for");
              build_ran_for.innerHTML = data["ran_for"];
              const build_status = document.getElementById("build-status");

              if (
                data["status"].startsWith("passed") ||
                data["status"].startsWith("failed")
              ) {
                build_ran_for.innerHTML = "Ran for " + data["ran_for"];
                const element_step_status = document.getElementById("build-status");
                if (data["status"].startsWith("passed")) {
                  element_step_status.innerHTML = iconSuccess;
                } else {
                  element_step_status.innerHTML = iconFailed;
                };
                if (data["can_cancel"]) {
                  document
                    .getElementById("rebuild-build")
                    .style.setProperty("display","none");
                  document
                    .getElementById("cancel-build")
                    .style.removeProperty("display");
                } else
                if (data["can_rebuild"]) {
                  document
                    .getElementById("cancel-build")
                    .style.setProperty("display","none");
                  document
                    .getElementById("rebuild-build")
                    .style.removeProperty("display");
                };
                console.log("Build has finished. Stop polling.");
              }

              // If the condition isn't met but the timeout hasn't elapsed, go again
              else if (Number(new Date()) < endTime) {
                if (
                  data["status"].startsWith("not started")
                ) {
                  build_status.innerHTML = iconQueued
                } else if (
                  data["status"].startsWith("active")
                ) {
                  build_status.innerHTML = iconActive
                };
                setTimeout(checkCondition, interval, resolve, reject);
              }
              // Didn't match and too much time, reject!
              else {
                reject(new Error("timed out for " + api_path + ": " + arguments));
              }
            });
        };

        return new Promise(checkCondition);
      }

      // Usage:  ensure element is visible
      poll(location.origin + "/api" + location.pathname);
|})
