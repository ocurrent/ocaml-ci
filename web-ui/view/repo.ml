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

  type repo_stats = {
    speed : float;
    reliability : float;
    build_frequency_per_week : float;
  }

  let row ~repo_title ~short_hash ~last_updated ~status ~description ~repo_uri
      ~statistics =
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
    let speed =
      let fmt v symbol =
        [
          txt (Printf.sprintf "%.1f" v);
          span ~a:[ a_class [ "text-sm pl-0.5" ] ] [ txt symbol ];
        ]
      in
      if Float.is_nan statistics.speed then [ txt "N/A" ]
      else if statistics.speed >= 3600. then
        fmt (statistics.speed /. 3600.) "hr"
      else if statistics.speed >= 60. then fmt (statistics.speed /. 60.) "min"
      else fmt statistics.speed "sec"
    in
    let reliability =
      if Float.is_nan statistics.reliability then [ txt "N/A" ]
      else
        [
          txt (Printf.sprintf "%.0f" (100. *. statistics.reliability));
          span ~a:[ a_class [ "text-sm pl-0.5" ] ] [ txt "%" ];
        ]
    in
    let build_frequency_per_week =
      if Float.is_nan statistics.build_frequency_per_week then [ txt "N/A" ]
      else
        [
          txt
            (Printf.sprintf "%.0f"
               (Float.ceil statistics.build_frequency_per_week));
          span ~a:[ a_class [ "text-sm pl-0.5" ] ] [ txt "/week" ];
        ]
    in
    ignore repo_uri;
    tr
      ~a:
        [
          a_class [ "cursor-pointer" ];
          (* a_onclick (Printf.sprintf "window.location='%s'" repo_uri); *)
          a_user_data "timestamp" last_updated_data;
        ]
      [
        td
          ~a:[ a_class [ "flex items-center space-x-3 " ] ]
          [
            Common.status_icon_build status;
            div
              ~a:[ a_class [ "text-sm space-y-1" ] ]
              [
                div
                  ~a:
                    [
                      a_class
                        [
                          "repo-title text-gray-900 dark:text-gray-200 text-sm \
                           font-medium";
                        ];
                    ]
                  [ txt repo_title ];
                info;
                div
                  ~a:[ a_class [ "text-gray-500 dark:text-gray-400" ] ]
                  [ div [ txt description ] ];
              ];
          ];
        td
          ~a:[ a_class [ "text-xs space-y-1 hidden md:table-cell" ] ]
          [
            div [ txt "master" ];
            div
              ~a:[ a_class [ "shadow-sm mb-4" ] ]
              [
                div
                  ~a:[ a_class [ "overflow-hidden" ] ]
                  [
                    div
                      ~a:[ a_class [ "bottom-0 inset-x-0 relative" ] ]
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
        td
          ~a:[ a_class [ "hidden md:table-cell" ] ]
          [ div ~a:[ a_class [ "text-2xl gray-700" ] ] speed ];
        td
          ~a:[ a_class [ "hidden md:table-cell" ] ]
          [ div ~a:[ a_class [ "text-2xl gray-700" ] ] reliability ];
        td
          ~a:[ a_class [ "hidden md:table-cell" ] ]
          [
            div ~a:[ a_class [ "text-2xl gray-700" ] ] build_frequency_per_week;
          ];
        td [ Common.right_arrow_head ];
      ]

  let repo_url org repo = Printf.sprintf "/%s/%s/%s" M.prefix org repo

  let table_head name =
    thead
      [
        tr
          [
            th [ div [ txt name ] ];
            th
              ~a:[ a_class [ "hidden md:table-cell" ] ]
              [ txt "Speed over time" ];
            th ~a:[ a_class [ "hidden md:table-cell" ] ] [ txt "Speed" ];
            th ~a:[ a_class [ "hidden md:table-cell" ] ] [ txt "Reliability" ];
            th
              ~a:[ a_class [ "hidden md:table-cell" ] ]
              [ txt "Build frequency" ];
            th [];
          ];
      ]

  let tabulate hd rows =
    div
      ~a:[ a_class [ "mt-8" ] ]
      [
        table
          ~a:
            [
              a_class
                [
                  "custom-table table-auto border border-gray-200 \
                   dark:border-gray-400 border-t-0 rounded-lg w-full min-w-0";
                ];
              a_id "table";
            ]
          ~thead:hd rows;
      ]

  let repo_name_compare { Client.Org.name = n0; _ } { Client.Org.name = n1; _ }
      =
    String.(compare (lowercase_ascii n0) (lowercase_ascii n1))

  let js_of_histories ~org data =
    let ( ++ ) a b = List.append a b in
    let commit_data { Client.Org.ran_for; _ } =
      Printf.sprintf "%s," (string_of_float (Option.value ~default:0. ran_for))
    in
    let commit_colour { Client.Org.status; _ } =
      match status with
      | Passed -> "\"rgba(18, 183, 106, 1)\","
      | Failed -> "\"rgba(217, 45, 32, 1)\","
      | _ -> "\"rgba(226, 232, 240, 1)\","
    in
    let commit_link repo i { Client.Org.hash; _ } =
      Printf.sprintf "%d:\"%s\","
        (15 - i - 1)
        (Url.commit_url M.prefix ~org ~repo ~hash)
    in
    let js_of_history_list fmt (name, data) =
      let data = List.filteri (fun i _ -> i < 15) data |> List.map fmt in
      [ "\""; name; "\":[" ] ++ data ++ [ "]," ]
    in
    let js_of_history_dict fmt (name, data) =
      let data =
        List.filteri (fun i _ -> i < 15) data |> List.mapi (fmt name)
      in
      [ "\""; name; "\":{" ] ++ data ++ [ "}," ]
    in
    (* The chart is left-to-right old-to-new, which is the opposite direction to the provided data *)
    let rev_data =
      List.map (fun (repo, history) -> (repo, List.rev history)) data
    in
    let chart_labels = List.init 15 (fun x -> Printf.sprintf "%d," (x + 1)) in
    let chart_data =
      List.map (js_of_history_list commit_data) rev_data |> List.flatten
    in
    let chart_colours =
      List.map (js_of_history_list commit_colour) rev_data |> List.flatten
    in
    let chart_links =
      List.map (js_of_history_dict commit_link) data |> List.flatten
    in
    [ "var chart_labels = [" ]
    ++ chart_labels
    ++ [ "]\nvar chart_data = {" ]
    ++ chart_data
    ++ [ "}\nvar chart_colours = {" ]
    ++ chart_colours
    ++ [ "}\nvar chart_links = {" ]
    ++ chart_links
    ++ [ "}\n" ]
    |> String.concat ""

  let build_frequency_per_week history =
    let ts =
      history
      |> List.filter_map (fun { Client.Org.started_at; _ } ->
             match started_at with
             | None -> None
             | Some v -> if Float.equal v 0. then None else Some v)
    in
    match ts with
    | [] -> 0.
    | ts ->
        let n_ts = List.length ts in
        let to_week f = float_of_int Duration.(to_day @@ of_f f) /. 7. in
        let now = Unix.gettimeofday () in
        let earliest = List.fold_left Float.min Float.max_float ts in
        to_week (now -. earliest) /. float_of_int n_ts

  let repo_statistics history =
    let n = List.length history in
    let sum_speed =
      List.fold_left
        (fun acc { Client.Org.ran_for; _ } ->
          acc +. Option.value ~default:0. ran_for)
        0. history
    in
    let n_succeeded, n_finished =
      List.fold_left
        (fun (n_succeeded, n_finished) { Client.Org.status; _ } ->
          match status with
          | Passed -> (n_succeeded + 1, n_finished + 1)
          | Failed -> (n_succeeded, n_finished + 1)
          | _ -> (n_succeeded, n_finished))
        (0, 0) history
    in
    let speed = sum_speed /. float_of_int n in
    let reliability = float_of_int n_succeeded /. float_of_int n_finished in
    let build_frequency_per_week = build_frequency_per_week history in
    { speed; reliability; build_frequency_per_week }

  let list ~org ~repos =
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
                      "custom-table table-auto border border-gray-200 \
                       border-t-0 rounded-lg w-full min-w-0";
                    ];
                  a_id "table";
                ]
              ~thead:hd rows;
          ])
    in
    let table_head name =
      thead
        [ tr [ th [ div [ txt name ] ]; th []; th []; th []; th []; th [] ] ]
    in
    let row ~repo_title ~short_hash ~last_updated ~status ~description ~repo_uri
        =
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
                        a_class
                          [
                            "repo-title text-gray-900 dark:text-gray-200 \
                             text-sm font-medium";
                          ];
                      ]
                    [ txt repo_title ];
                  info;
                  div
                    ~a:[ a_class [ "text-grey-500 dark:text-gray-400" ] ]
                    [ div [ txt description ] ];
                ];
            ];
          td ~a:[ a_class [ "text-xs space-y-1" ] ] [];
          td [];
          td [];
          td [];
          td [ Common.right_arrow_head ];
        ]
    in
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
        Tyxml.Html.script ~a:[ a_src "/js/repo-page.js" ] (txt "");
        Common.breadcrumbs [ (M.prefix, M.prefix) ] org;
        title ~org;
        tabulate table_head table;
      ]

  let list_new ~org ~repos ~histories =
    let table_head =
      table_head (Printf.sprintf "Repositories (%d)" (List.length repos))
    in
    let table =
      let f { Client.Org.name; main_status; main_hash; main_last_updated } =
        let history =
          snd @@ List.find (fun (repo, _) -> String.equal name repo) histories
        in
        row ~repo_title:name
          ~short_hash:(Common.short_hash main_hash)
          ~last_updated:main_last_updated ~status:main_status ~description:""
          ~repo_uri:(repo_url org name) ~statistics:(repo_statistics history)
      in
      List.map f (List.sort repo_name_compare repos)
    in
    Template_v1.instance
      [
        script
          ~a:
            [
              a_src
                "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.0.1/chart.umd.js";
            ]
          (txt "");
        script (Unsafe.data (js_of_histories ~org histories));
        script ~a:[ a_src "/js/repo-page.js" ] (txt "");
        Common.breadcrumbs [ (M.prefix, M.prefix) ] org;
        title ~org;
        tabulate table_head table;
      ]
end
