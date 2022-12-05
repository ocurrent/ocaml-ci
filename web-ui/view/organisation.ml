open Git_forge

module Make (M : Forge_prefix) = struct
  let profile_picture org =
    (* /profile-pictures is where images are downloaded -- see Dockerfile.web *)
    let local_image =
      Printf.sprintf "/profile-pictures/%s/%s.png" M.prefix org
    in
    let fallback_image = Printf.sprintf "/images/%s-logo-500.png" M.prefix in
    let local_image_exists =
      (* TODO fix error when not in deployement *)
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
