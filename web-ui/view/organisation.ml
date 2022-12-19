open Git_forge

module Make (M : Forge_prefix) = struct
  module Client = Client

  let profile_picture org =
    (* /profile-pictures is where images are downloaded -- see Dockerfile.web *)
    let local_image =
      Printf.sprintf "/profile-pictures/%s/%s.png" M.prefix org
    in
    let fallback_image = Printf.sprintf "/images/%s-logo-500.png" M.prefix in
    let local_image_exists =
      match Bos.OS.File.exists (Fpath.v local_image) with
      | Ok b -> b
      | Error _ -> false
    in
    Logs.warn (fun l -> l "Looking for %s@." local_image);
    let url = if local_image_exists then local_image else fallback_image in
    Logs.warn (fun l -> l "Image is %S" url);
    Tyxml.Html.(
      img
        ~a:[ a_class [ "w-20 h-20 rounded-full" ] ]
        ~src:url
        ~alt:(Printf.sprintf "%s profile picture" org)
        ())

  let logo =
    match M.prefix with
    | "github" -> Common.github_logo
    | "gitlab" -> Common.gitlab_logo
    | _ -> raise Not_found

  let git_forge_url org = Printf.sprintf "https://%s.com/%s" M.prefix org

  let row ~org =
    let repo_symbol =
      Tyxml.Svg.(
        Tyxml.Html.svg
          ~a:
            [
              a_class [ "h-5 w-5 rotate-180" ];
              a_viewBox (0., 0., 20., 20.);
              a_fill (`Color ("#FFFFFF", None));
              a_stroke (`Color ("#344054", None));
              a_stroke_width (1., Some `Px);
            ]
          [
            path
              ~a:
                [
                  Tyxml_helpers.a_svg_custom "fill-rule" "evenodd";
                  a_d
                    "M2 5a2 2 0 012-2h12a2 2 0 012 2v2a2 2 0 01-2 2H4a2 2 0 \
                     01-2-2V5zm14 1a1 1 0 11-2 0 1 1 0 012 0zM2 13a2 2 0 \
                     012-2h12a2 2 0 012 2v2a2 2 0 01-2 2H4a2 2 0 01-2-2v-2zm14 \
                     1a1 1 0 11-2 0 1 1 0 012 0z";
                  Tyxml_helpers.a_svg_custom "clip-rule" "evenodd";
                ]
              [];
          ])
    in
    let name = org.Client.CI.name in
    let repository_count =
      if org.number_repos = 1 then "1 repository"
      else Printf.sprintf "%d repositories" org.number_repos
    in
    let org_url = Url.org_url M.prefix ~org:name in
    Tyxml.Html.(
      a
        ~a:[ a_class [ "item-card flex" ]; a_href org_url ]
        [
          div
            ~a:[ a_class [ "data-info" ]; a_style "display: none" ]
            [ txt M.prefix ];
          profile_picture name;
          div
            ~a:
              [
                a_class
                  [
                    "flex flex-row md:flex-col items-center md:items-start  \
                     space-x-4 md:space-x-0 mx-4 flex-wrap truncate";
                  ];
              ]
            [
              div
                ~a:
                  [
                    a_class
                      [
                        "org-title font-semibold text-2xl md:text-lg mb-1 \
                         truncate";
                      ];
                  ]
                [ txt name ];
              div ~a:[ a_class [ "text-sm" ] ] [];
              div
                ~a:
                  [
                    a_class
                      [
                        "flex mt-0 md:mt-4 text-sm text-gray-700 \
                         dark:text-gray-400 font-normal space-x-4";
                      ];
                  ]
                [
                  logo;
                  div
                    ~a:[ a_class [ "hidden xl:inline" ] ]
                    [ txt (git_forge_url name) ];
                  div
                    ~a:[ a_class [ "flex items-center space-x-2" ] ]
                    [ repo_symbol; div [ txt repository_count ] ];
                ];
            ];
        ])
end
