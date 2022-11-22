open Git_forge

module Make (M : M_Git_forge) = struct
  let profile_picture_url org =
    (* FIXME [benmandrew]: How can we get the GitLab profile pictures? *)
    match M.prefix with
    | "github" -> Printf.sprintf "https://github.com/%s.png?size=88" org
    | "gitlab" -> "/images/gitlab-logo-500.png"
    | _ -> ""

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
    let org_url = Url.org_url M.prefix ~org in
    Tyxml.Html.(
      a
        ~a:[ a_class [ "item-card flex space-x-4" ]; a_href org_url ]
        [
          img
            ~a:[ a_class [ "w-20 h-20 rounded-full" ] ]
            ~src:(profile_picture_url org)
            ~alt:(Printf.sprintf "%s profile picture" org)
            ();
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
