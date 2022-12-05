open Tyxml.Html
module Client = Git_forge_intf.Client

module Make (M : Git_forge_intf.Forge) = struct
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
    let url = if local_image_exists then local_image else fallback_image in
    img
      ~a:
        [
          a_class [ "w-20 h-20 rounded-full" ];
          a_style "border-radius: 50%; width: 80px";
        ]
      ~src:url
      ~alt:(Printf.sprintf "%s profile picture" org)
      ()

  let title ~org =
    let org_url = M.org_url ~org in
    div
      ~a:
        [
          a_class [ "flex flex-col md:flex-row justify-between items-center " ];
        ]
      [
        div
          ~a:[ a_class [ "flex space-x-4" ] ]
          [
            profile_picture org;
            div
              ~a:[ a_class [ "flex flex-col truncate" ] ]
              [
                h1 ~a:[ a_class [ "text-xl" ] ] [ txt org ];
                a
                  ~a:
                    [
                      a_class [ "text-sm flex items-center space-x-2" ];
                      a_href org_url;
                    ]
                  [
                    span ~a:[ a_class [ "truncate" ] ] [ txt org_url ];
                    Common.external_link;
                  ];
              ];
          ];
        div
          ~a:[ a_class [ "flex items-center justify-between space-x-3" ] ]
          [
            div
              ~a:[ a_class [ "form-control relative max-w-80 py-6 md:py-0" ] ]
              [
                Common.search;
                input
                  ~a:
                    [
                      a_input_type `Text;
                      a_placeholder "Search for a repository";
                      a_oninput "search(this.value)";
                    ]
                  ();
              ];
            div
              ~a:[ a_class [ "relative" ]; a_style "display:none" ]
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
    let info =
      let hash = span ~a:[ a_class [ "font-medium" ] ] [ txt short_hash ] in
      match last_updated with
      | None -> div [ hash ]
      | Some _ ->
          div
            [
              hash;
              txt
                (Printf.sprintf " on %s"
                   (Timestamps_durations.pp_timestamp last_updated));
            ]
    in
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
                info;
                div
                  ~a:[ a_class [ "text-grey-500" ] ]
                  [ div [ txt description ] ];
              ];
          ];
        td
          ~a:[ a_class [ "text-xs space-y-1" ] ]
          [
            div [ txt "master" ];
            div
              ~a:[ a_class [ "shadow-sm mb-4" ] ]
              [
                div
                  ~a:[ a_class [ "overflow-hidden" ] ]
                  [
                    div
                      ~a:[ a_class [ "bottom-0 inset-x-0" ] ]
                      [
                        canvas
                          ~a:
                            [
                              a_id (Printf.sprintf "chart_%s" repo_title);
                              a_height 28;
                            ]
                          [];
                      ];
                  ];
              ];
          ];
        td [];
        td [];
        td [];
        td [ Common.right_arrow_head ];
      ]

  let repo_url org repo = Printf.sprintf "/%s/%s/%s" M.prefix org repo

  let table_head name =
    thead
      [
        tr
          [
            th [ div [ txt name ] ];
            th [ txt "Speed over time" ];
            th [ txt "Speed" ];
            th [ txt "Reliability" ];
            th [ txt "Build frequency" ];
            th [];
          ];
      ]

  let tabulate hd rows =
    Tyxml.Html.(
      div
        ~a:[ a_class [ "mt-8" ] ]
        [
          table
            ~a:
              [
                a_class
                  [
                    "custom-table table-auto border border-gray-200 border-t-0 \
                     rounded-lg w-full min-w-0";
                  ];
                a_id "table";
              ]
            ~thead:hd rows;
        ])

  let repo_name_compare { Client.Org.name = n0; _ } { Client.Org.name = n1; _ }
      =
    String.(compare (lowercase_ascii n0) (lowercase_ascii n1))

  let js_of_histories data =
    let ( ++ ) a b = List.append a b in
    let commit_data { Client.Repo.ran_for; _ } =
      string_of_float (Option.value ~default:0. ran_for)
    in
    let commit_colour { Client.Repo.status; _ } =
      match status with
      | Passed -> "rgba(18, 183, 106, 1)"
      | Failed -> "rgba(217, 45, 32, 1)"
      | _ -> "rgba(226, 232, 240, 1)"
    in
    let js_of_history fmt (name, data) =
      let data = List.map fmt data in
      [ "\""; name; "\":[" ] ++ data ++ [ "]," ]
    in
    let chart_labels = List.init 15 (fun x -> Printf.sprintf "%d," (x + 1)) in
    let chart_data =
      List.map (js_of_history commit_data) data |> List.flatten
    in
    let chart_colours =
      List.map (js_of_history commit_colour) data |> List.flatten
    in
    [ "var chart_labels = {" ]
    ++ chart_labels
    ++ [ "}\nvar chart_data = {" ]
    ++ chart_data
    ++ [ "}\nvar chart_colours = {" ]
    ++ chart_colours
    ++ [ "}" ]
    |> String.concat ""

  let list ~org ~repos =
    let table_head =
      table_head (Printf.sprintf "Repositories (%d)" (List.length repos))
    in
    let table =
      let f { Client.Org.name; main_status; main_hash; main_last_updated } =
        row ~repo_title:name
          ~short_hash:(Common.short_hash main_hash)
          ~last_updated:main_last_updated ~status:main_status ~description:""
          ~repo_uri:(repo_url org name)
      in
      List.map f (List.sort repo_name_compare repos)
    in
    Template_v1.instance
      [
        script
          ~a:
            [
              a_src
                "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.3/Chart.min.js";
            ]
          (txt "");
        (* script (txt (js_of_histories histories)); *)
        script ~a:[ a_src "/js/repo-page-search.js" ] (txt "");
        Common.breadcrumbs [ (M.prefix, M.prefix) ] org;
        title ~org;
        tabulate table_head table;
      ]
end
