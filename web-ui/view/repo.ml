let row ~repo_title ~short_hash ~last_updated ~status ~repo_uri =
  ignore last_updated;
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
                      [ div [ txt short_hash ] ];
                  ];
              ];
          ];
      ])
