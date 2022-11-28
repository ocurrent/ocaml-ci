open Git_forge

module Make (M : M_Git_forge) = struct
  let profile_picture org =
    (* FIXME [benmandrew]: How can we get the GitLab profile pictures? *)
    let local_image =
      match M.prefix with
      | "github" -> Printf.sprintf "/images/profile-pictures/github/%s.png" org
      | "gitlab" -> "/images/gitlab-logo-500.png"
      | _ -> ""
    in
    let fallback_image = Printf.sprintf "/images/%s-logo-500.png" M.prefix in
    let local_image_exists =
      try
        let _ = Unix.stat (Printf.sprintf "./web-ui/static%s" local_image) in
        true
      with _ -> false
    in
    let url = if local_image_exists then local_image else fallback_image in
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

  let git_forge_url org =
    match M.prefix with
    | "github" -> Printf.sprintf "https://github.com/%s" org
    | "gitlab" -> Printf.sprintf "https://gitlab.com/%s" org
    | _ -> raise Not_found

  let row ~org =
    Dream.log "WHAT";
    let org_url = Url.org_url M.prefix ~org in
    Tyxml.Html.(
      a
        ~a:[ a_class [ "item-card flex space-x-4" ]; a_href org_url ]
        [
          div
            ~a:[ a_class [ "data-info" ]; a_style "display: none" ]
            [ txt M.prefix ];
          profile_picture org;
          div
            ~a:[ a_class [ "flex flex-col" ] ]
            [
              div
                ~a:[ a_class [ "org-title font-semibold text-lg mb-1" ] ]
                [ txt org ];
              div ~a:[ a_class [ "text-sm" ] ] [];
              div
                ~a:
                  [
                    a_class
                      [
                        "flex mt-4 text-sm text-gray-700 font-normal space-x-4";
                      ];
                  ]
                [ logo; div [ txt (git_forge_url org) ] ];
            ];
        ])
end
