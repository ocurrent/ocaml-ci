let row ~prefix ~owner ~description ~n_repos ~image ~url =
  let org_url org = Printf.sprintf "/%s/%s" prefix org in
  Tyxml.Html.(
    a
      ~a:[ a_href (org_url owner); a_class [ "item-card flex space-x-4" ] ]
      [
        image;
        div
          ~a:[ a_class [ "flex flex-col" ] ]
          [
            div ~a:[ a_class [ "font-semibold text-lg mb-1" ] ] [ txt owner ];
            (* FIXME [benmandrew]: [description] here, currently only placeholder exists *)
            div ~a:[ a_class [ "text-sm" ] ] [ txt description ];
            div
              ~a:
                [
                  a_class
                    [ "flex mt-4 text-sm text-gray-700 font-normal space-x-4" ];
                ]
              [
                div [ txt url ];
                div
                  ~a:[ a_class [ "flex items-center space-x-2" ] ]
                  [
                    Common.repositories;
                    div [ txt (Printf.sprintf "%d repositories" n_repos) ];
                  ];
              ];
          ];
      ])
