let title ~org =
  let github_org_url = Printf.sprintf "https://github.com/%s" org in
  Tyxml.Html.(
    div
      ~a:[ a_class [ "justify-between items-center flex" ] ]
      [
        div
          ~a:[ a_class [ "flex space-x-4" ] ]
          [
            img
              ~a:[ a_style "border-radius: 50%; width: 88px" ]
              ~src:(Printf.sprintf "https://github.com/%s.png?size=200" org)
              ~alt:(Printf.sprintf "%s profile picture" org)
              ();
            div
              ~a:[ a_class [ "flex flex-col" ] ]
              [
                h1 ~a:[ a_class [ "text-xl" ] ] [ txt org ];
                a
                  ~a:
                    [
                      a_class [ "text-sm flex items-center space-x-2" ];
                      a_href github_org_url;
                    ]
                  [ span [ txt github_org_url ]; Common.external_link ];
              ];
          ];
      ])

let row ~repo_title ~short_hash ~last_updated ~status ~description ~repo_uri =
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
              ~a:[ a_class [ "text-sm space-y-1" ] ]
              [
                div
                  ~a:[ a_class [ "text-gray-900 text-sm font-medium" ] ]
                  [ txt repo_title ];
                div [ span ~a:[ a_class [ "font-medium" ] ] [ txt short_hash ] ];
                div
                  ~a:[ a_class [ "text-grey-500" ] ]
                  [ div [ txt description ] ];
              ];
          ];
      ])
