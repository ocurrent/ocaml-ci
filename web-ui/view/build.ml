let title_card ~status ~card_title ~hash_link ~ref_link ~first_created_at
    ~ran_for ~buttons =
  Tyxml.Html.(
    div
      ~a:[ a_class [ "justify-between items-center flex" ] ]
      [
        div
          ~a:[ a_class [ "flex items-center space-x-4" ] ]
          [
            Common.status_icon status;
            div
              ~a:[ a_class [ "flex flex-col space-y-1" ] ]
              [
                div
                  ~a:[ a_class [ "flex items-center" ] ]
                  [
                    h1 ~a:[ a_class [ "text-xl" ] ] [ txt card_title ];
                    a
                      ~a:
                        [
                          a_class [ "btn btn-secondary btn-sm ml-4" ];
                          a_href "#";
                        ]
                      [ txt "View Build History" ];
                  ];
                div
                  ~a:[ a_class [ "text-gray-500" ] ]
                  [
                    div
                      ~a:[ a_class [ "flex space-x-2 text-sm" ] ]
                      [
                        div [ hash_link ];
                        div [ txt "-" ];
                        div [ txt first_created_at ];
                        div [ txt "-" ];
                        div [ ref_link ];
                      ];
                  ];
              ];
          ];
        div
          ~a:[ a_class [ "flex items-center justify-between space-x-4" ] ]
          [
            div
              ~a:[ a_class [ "text-sm" ] ]
              [ txt @@ Fmt.str "Ran for %s" ran_for ];
            div
              ~a:
                [
                  a_class [ "relative" ];
                  Tyxml_helpers.x_data "{rebuildMenu: false}";
                ]
              buttons;
          ];
      ])

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
          [ div [ txt @@ Fmt.str "Ran for %s" ran_for ]; Common.right_arrow_head ];
      ])

let tabulate_steps step_rows =
  Tyxml.Html.(
    div
      ~a:[ a_class [ "container-fluid mt-8 flex flex-col space-y-6" ] ]
      [ div ~a:[ a_class [ "table-container" ] ] step_rows ])
