open Tyxml.Html
open Git_forge

module Make (M : M_Git_forge) = struct
  let profile_picture_url org =
    (* FIXME [benmandrew]: How can we get the GitLab profile pictures? *)
    match M.prefix with
    | "github" -> Printf.sprintf "https://github.com/%s.png?size=200" org
    | _ -> ""

  let org_url org =
    match M.prefix with
    | "github" -> Printf.sprintf "https://github.com/%s" org
    | "gitlab" -> Printf.sprintf "https://gitlab.com/%s" org
    | _ -> raise Not_found

  let title ~org =
    let org_url = org_url org in
    div
      ~a:[ a_class [ "justify-between items-center flex" ] ]
      [
        div
          ~a:[ a_class [ "flex space-x-4" ] ]
          [
            img
              ~a:
                [
                  a_class [ "w-20 h-20" ];
                  a_style "border-radius: 50%; width: 80px";
                ]
              ~src:(profile_picture_url org)
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
                      a_href org_url;
                    ]
                  [ span [ txt org_url ]; Common.external_link ];
              ];
          ];
        div
          ~a:[ a_class [ "flex items-center justify-between space-x-3" ] ]
          [
            div
              ~a:[ a_class [ "form-control relative w-80" ] ]
              [
                Common.search;
                input
                  ~a:
                    [
                      a_input_type `Text;
                      a_placeholder "Search for a repository";
                      a_onchange "search(this.value)";
                    ]
                  ();
              ];
            div
              ~a:[ a_class [ "relative" ] ]
              [
                select
                  ~a:
                    [
                      a_class
                        [
                          "input-control relative input-text text-gray-500 \
                           items-center justify-between flex px-3 py-2 \
                           appearance-none";
                        ];
                      a_name "Languages";
                      a_onchange "sort(this.value)";
                    ]
                  [
                    option ~a:[ a_value "alpha" ] (txt "Alphabetical");
                    option ~a:[ a_value "recent" ] (txt "Recent");
                  ];
              ];
          ];
      ]

  let row ~repo_title ~short_hash ~last_updated ~status ~description ~repo_uri =
    let last_updated_fmt = Timestamps_durations.pp_timestamp last_updated in
    (* Defaulting infinity means sorting by recent places them at the bottom of the page *)
    let last_updated_data =
      match last_updated with
      | None -> "Infinity"
      | Some v -> Printf.sprintf "%f" v
    in
    tr
      ~a:
        [
          a_class [ "cursor-pointer" ];
          a_onclick (Printf.sprintf "window.location='%s'" repo_uri);
          a_user_data "timestamp" last_updated_data;
        ]
      [
        td
          ~a:[ a_class [ "flex items-center space-x-3" ] ]
          [
            Common.status_icon_build status;
            div
              ~a:[ a_class [ "text-sm space-y-1" ] ]
              [
                div
                  ~a:
                    [
                      a_class [ "repo-title text-gray-900 text-sm font-medium" ];
                    ]
                  [ txt repo_title ];
                div
                  [
                    span ~a:[ a_class [ "font-medium" ] ] [ txt short_hash ];
                    txt (Printf.sprintf " on %s" last_updated_fmt);
                  ];
                div
                  ~a:[ a_class [ "text-grey-500" ] ]
                  [ div [ txt description ] ];
              ];
          ];
        td ~a:[ a_class [ "text-xs space-y-1" ] ] [];
        td [];
        td [];
        td [];
        td [ Common.right_arrow_head ];
      ]

  let repo_url org repo = Fmt.str "/%s/%s/%s" M.prefix org repo

  let list ~org ~repos =
    let table_head =
      Common.table_head_tr
        (Printf.sprintf "Repositories (%d)" (List.length repos))
    in
    let table =
      let f { Client.Org.name; main_status; main_hash; main_last_updated } =
        row ~repo_title:name ~short_hash:(short_hash main_hash)
          ~last_updated:main_last_updated ~status:main_status ~description:""
          ~repo_uri:(repo_url org name)
      in
      List.map f repos
    in
    Template_v1.instance
      [
        Tyxml.Html.script ~a:[ a_src "/js/repo-page-search.js" ] (txt "");
        Common.breadcrumbs [ (M.prefix, M.prefix) ] org;
        title ~org;
        Common.tabulate_tr table_head table;
      ]
end
