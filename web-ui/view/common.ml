module Run_time = Ocaml_ci.Run_time

let short_hash = Astring.String.with_range ~len:6

let rec intersperse ~sep = function
  | [] -> []
  | [ x ] -> [ x ]
  | x :: xs -> x :: sep :: intersperse ~sep xs

let failing_red = "#D92D20"
let success_green = "#12B76A"

let icon_failed =
  Tyxml.Html.(
    div
      ~a:[ a_class [ "icon-status icon-status--failed" ] ]
      [
        Tyxml.Svg.(
          Tyxml.Html.svg
            ~a:
              [
                a_class [ "h-3 w-3" ];
                a_viewBox (0., 0., 20., 20.);
                a_fill (`Color (failing_red, None));
              ]
            [
              path
                ~a:
                  [
                    Tyxml_helpers.a_svg_custom "fill-rule" "evenodd";
                    a_d
                      "M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 \
                       111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 \
                       1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 \
                       10 4.293 5.707a1 1 0 010-1.414z";
                    Tyxml_helpers.a_svg_custom "clip-rule" "evenodd";
                  ]
                [];
            ]);
      ])

let icon_success =
  Tyxml.Html.(
    div
      ~a:[ a_class [ "icon-status icon-status--success" ] ]
      [
        Tyxml.Svg.(
          Tyxml.Html.svg
            ~a:
              [
                a_class [ "h-4 w-4" ];
                a_viewBox (0., 0., 20., 20.);
                a_fill (`Color (success_green, None));
              ]
            [
              path
                ~a:
                  [
                    Tyxml_helpers.a_svg_custom "fill-rule" "evenodd";
                    a_d
                      "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 \
                       0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 \
                       011.414 0z";
                    Tyxml_helpers.a_svg_custom "clip-rule" "evenodd";
                  ]
                [];
            ]);
      ])

let icon_active =
  Tyxml.Html.(
    div ~a:[ a_class [ "icon-status icon-status--active" ] ] [ div [] ])

let icon_queued =
  Tyxml.Html.(
    div ~a:[ a_class [ "icon-status icon-status--default" ] ] [ div [] ])

let status_icon (status : Ocaml_ci_api.Client.State.t) =
  match status with
  | NotStarted -> icon_queued
  | Failed _ | Aborted -> icon_failed
  | Passed -> icon_success
  | Active -> icon_active
  | Undefined _ -> icon_failed

let status_icon_build (status : Build_status.t) =
  match status with
  | NotStarted -> icon_queued
  | Failed -> icon_failed
  | Passed -> icon_success
  | Pending -> icon_active
  | Undefined _ -> icon_failed

(* https://grantw.uk/articles/submit-a-html-form-with-alpine-js/ *)
let form_for ~x_ref ~action ~csrf_token ~submit_button ~input_value =
  Tyxml.Html.(
    form
      ~a:[ Tyxml_helpers.x_ref x_ref; a_action action; a_method `Post ]
      [
        Unsafe.data csrf_token;
        submit_button;
        input
          ~a:[ a_name "filter"; a_input_type `Hidden; a_value input_value ]
          ();
      ])

let form_cancel_step ~variant ~csrf_token ?(show = true) () =
  let display_none = if show then "" else "display:none" in
  let submit_button =
    Tyxml.Html.(
      button
        [ txt "Cancel" ]
        ~a:
          [
            a_id "cancel-step";
            a_class [ "btn btn-primary" ];
            a_style display_none;
            Tyxml_helpers.at_click "$refs.cancelStepForm.submit()";
          ])
  in
  form_for ~csrf_token ~x_ref:"cancelForm"
    ~action:(Dream.to_percent_encoded variant ^ "/cancel")
    ~submit_button ~input_value:"Cancel"

let form_rebuild_step ~variant ~csrf_token ?(show = true) () =
  let display_none = if show then "" else "display:none" in
  let submit_button =
    Tyxml.Html.(
      button
        [ txt "Rebuild" ]
        ~a:
          [
            a_id "rebuild-step";
            a_class [ "btn btn-primary" ];
            a_style display_none;
            Tyxml_helpers.at_click "$refs.rebuildStepForm.submit()";
          ])
  in
  form_for ~csrf_token ~submit_button ~x_ref:"rebuildStepForm"
    ~action:(Dream.to_percent_encoded variant ^ "/rebuild")
    ~input_value:"Rebuild"

let form_rebuild_all ~hash ~csrf_token =
  let submit_button =
    Tyxml.Html.(
      button
        [ txt "Rebuild All" ]
        ~a:[ Tyxml_helpers.at_click "$refs.rebuildAllForm.submit()" ])
  in

  form_for ~csrf_token ~submit_button ~x_ref:"rebuildAllForm"
    ~action:(hash ^ "/rebuild-all") ~input_value:"none"

let form_rebuild_failed ~hash ~csrf_token =
  let submit_button =
    Tyxml.Html.(
      button
        [ txt "Rebuild Failed" ]
        ~a:[ Tyxml_helpers.at_click "$refs.rebuildFailedForm.submit()" ])
  in
  form_for ~csrf_token ~x_ref:"rebuildFailedForm"
    ~action:(hash ^ "/rebuild-failed") ~submit_button ~input_value:"failed"

let form_cancel ~hash ~csrf_token ?(show = true) () =
  let display_none = if show then "" else "display:none" in
  let submit_button =
    Tyxml.Html.(
      button
        [ txt "Cancel" ]
        ~a:
          [
            a_id "cancel-build";
            a_class [ "btn btn-primary" ];
            a_style display_none;
            Tyxml_helpers.at_click "$refs.cancelForm.submit()";
          ])
  in
  form_for ~csrf_token ~x_ref:"cancelForm" ~action:(hash ^ "/cancel")
    ~submit_button ~input_value:"Cancel"

let rebuild_button ~hash ~csrf_token ?(show = true) () =
  let display_none = if show then "" else "display:none" in
  [
    Tyxml.Html.(
      button
        ~a:
          [
            a_id "rebuild-build";
            a_class [ "btn btn-primary" ];
            a_style display_none;
            Tyxml_helpers.at_click "rebuildMenu = !rebuildMenu";
            Tyxml_helpers.at_click_outside "rebuildMenu = false";
            a_aria "label" [ "Rebuild" ];
          ]
        [
          txt "Rebuild";
          svg
            ~a:
              [
                Tyxml.Svg.a_class [ "h-4 w-4" ];
                Tyxml.Svg.a_viewBox (0., 0., 20., 20.);
                Tyxml.Svg.a_fill (`Color ("#FFFFFF", None));
              ]
            [
              Tyxml.Svg.path
                ~a:
                  [
                    Tyxml_helpers.a_svg_custom "fill-rule" "evenodd";
                    Tyxml.Svg.a_d
                      "M16.707 10.293a1 1 0 010 1.414l-6 6a1 1 0 01-1.414 \
                       0l-6-6a1 1 0 111.414-1.414L9 14.586V3a1 1 0 012 \
                       0v11.586l4.293-4.293a1 1 0 011.414 0z";
                    Tyxml_helpers.a_svg_custom "clip-rule" "evenodd";
                  ]
                [];
            ];
        ]);
    Tyxml.Html.(
      div
        ~a:
          [
            a_class [ "dropdown-menu mt-2" ];
            Tyxml_helpers.x_show "rebuildMenu";
            Tyxml_helpers.x_transition;
          ]
        [
          form_rebuild_all ~hash ~csrf_token;
          form_rebuild_failed ~hash ~csrf_token;
        ]);
  ]

let right_arrow_head =
  Tyxml.Svg.(
    Tyxml.Html.svg
      ~a:
        [
          a_class [ "hidden md:inline h-5 w-5 -rotate-90" ];
          a_fill `None;
          a_viewBox (0., 0., 24., 24.);
          a_stroke `CurrentColor;
          a_stroke_width (2., Some `Px);
        ]
      [
        path
          ~a:
            [
              a_stroke_linecap `Round;
              a_stroke_linejoin `Round;
              a_d "M19 9l-7 7-7-7";
            ]
          [];
      ])

let external_link =
  Tyxml.Svg.(
    Tyxml.Html.svg
      ~a:
        [
          a_class [ "w-4 h-4" ];
          a_fill `None;
          a_viewBox (0., 0., 13., 14.);
          a_stroke (`Color ("currentColor", None));
          a_stroke_width (1.25, Some `Px);
        ]
      [
        path
          ~a:
            [
              a_stroke_linecap `Round;
              a_stroke_linejoin `Round;
              a_d
                "M10.25 7.625V11.375C10.25 11.7065 10.1183 12.0245 9.88388 \
                 12.2589C9.64946 12.4933 9.33152 12.625 9 12.625H2.125C1.79348 \
                 12.625 1.47554 12.4933 1.24112 12.2589C1.0067 12.0245 0.875 \
                 11.7065 0.875 11.375V4.5C0.875 4.16848 1.0067 3.85054 1.24112 \
                 3.61612C1.47554 3.3817 1.79348 3.25 2.125 3.25H5.875M8.375 \
                 1.375H12.125M12.125 1.375V5.125M12.125 1.375L5.25 8.25";
            ]
          [];
      ])

let search =
  Tyxml.Svg.(
    Tyxml.Html.svg
      ~a:
        [
          (* a_class [ "w-4 h-4" ]; *)
          a_fill `None;
          a_viewBox (0., 0., 24., 24.);
          a_stroke (`Color ("#667085", None));
          a_stroke_width (2., Some `Px);
        ]
      [
        path
          ~a:
            [
              a_stroke_linecap `Round;
              a_stroke_linejoin `Round;
              a_d "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z";
            ]
          [];
      ])

let github_logo =
  Tyxml.Svg.(
    Tyxml.Html.svg
      ~a:[ a_class [ "w-4 h-4" ]; a_fill `None; a_viewBox (0., 0., 15., 15.) ]
      [
        path
          ~a:
            [
              a_class [ "svg-fill" ];
              a_d
                "M7 0C3.1325 0 0 3.1325 0 7C0 10.0975 2.00375 12.7137 4.78625 \
                 13.6413C5.13625 13.7025 5.2675 13.4925 5.2675 13.3088C5.2675 \
                 13.1425 5.25875 12.5913 5.25875 12.005C3.5 12.3288 3.045 \
                 11.5763 2.905 11.1825C2.82625 10.9812 2.485 10.36 2.1875 \
                 10.1937C1.9425 10.0625 1.5925 9.73875 2.17875 9.73C2.73 \
                 9.72125 3.12375 10.2375 3.255 10.4475C3.885 11.5062 4.89125 \
                 11.2088 5.29375 11.025C5.355 10.57 5.53875 10.2638 5.74 \
                 10.0887C4.1825 9.91375 2.555 9.31 2.555 6.6325C2.555 5.87125 \
                 2.82625 5.24125 3.2725 4.75125C3.2025 4.57625 2.9575 3.85875 \
                 3.3425 2.89625C3.3425 2.89625 3.92875 2.7125 5.2675 \
                 3.61375C5.8275 3.45625 6.4225 3.3775 7.0175 3.3775C7.6125 \
                 3.3775 8.2075 3.45625 8.7675 3.61375C10.1063 2.70375 10.6925 \
                 2.89625 10.6925 2.89625C11.0775 3.85875 10.8325 4.57625 \
                 10.7625 4.75125C11.2087 5.24125 11.48 5.8625 11.48 \
                 6.6325C11.48 9.31875 9.84375 9.91375 8.28625 10.0887C8.54 \
                 10.3075 8.75875 10.7275 8.75875 11.3837C8.75875 12.32 8.75 \
                 13.0725 8.75 13.3088C8.75 13.4925 8.88125 13.7113 9.23125 \
                 13.6413C10.6209 13.1721 11.8284 12.279 12.6839 \
                 11.0877C13.5393 9.89631 13.9996 8.46668 14 7C14 3.1325 \
                 10.8675 0 7 0Z";
            ]
          [];
      ])

let gitlab_logo =
  Tyxml.Svg.(
    Tyxml.Html.svg
      ~a:[ a_class [ "w-4 h-4" ]; a_fill `None; a_viewBox (0., 0., 380., 380.) ]
      [
        path
          ~a:
            [
              a_class [ "svg-fill" ];
              a_d
                "M282.83,170.73l-.27-.69-26.14-68.22a6.81,6.81,0,0,0-2.69-3.24,7,7,0,0,0-8,.43,7,7,0,0,0-2.32,3.52l-17.65,54H154.29l-17.65-54A6.86,6.86,0,0,0,134.32,99a7,7,0,0,0-8-.43,6.87,6.87,0,0,0-2.69,3.24L97.44,170l-.26.69a48.54,48.54,0,0,0,16.1,56.1l.09.07.24.17,39.82,29.82,19.7,14.91,12,9.06a8.07,8.07,0,0,0,9.76,0l12-9.06,19.7-14.91,40.06-30,.1-.08A48.56,48.56,0,0,0,282.83,170.73Z";
            ]
          [];
      ])

let information_icon =
  Tyxml.Svg.(
    Tyxml.Html.svg
      ~a:
        [
          a_class [ "w-3 h-3" ];
          a_fill `CurrentColor;
          a_viewBox (0., 0., 20., 20.);
        ]
      [
        path
          ~a:
            [
              Tyxml_helpers.a_svg_custom "fill-rule" "evenodd";
              a_d
                "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 \
                 012 0zM9 9a.75.75 0 000 1.5h.253a.25.25 0 01.244.304l-.459 \
                 2.066A1.75 1.75 0 0010.747 15H11a.75.75 0 \
                 000-1.5h-.253a.25.25 0 01-.244-.304l.459-2.066A1.75 1.75 0 \
                 009.253 9H9z";
              Tyxml_helpers.a_svg_custom "clip-rule" "evenodd";
            ]
          [];
      ])

(* https://www.kindacode.com/article/tailwind-css-how-to-create-tooltips/ *)
let tooltip message content =
  let down_arrow =
    "after:content-[''] after:absolute after:left-1/2 after:top-[100%] \
     after:-translate-x-1/2 after:border-8 after:border-x-transparent \
     after:border-b-transparent after:border-t-gray-700"
  in
  Tyxml.Html.(
    div ~a:[]
      [
        a
          ~a:
            [
              a_class
                [
                  "group relative inline flex flex-nowrap items-center \
                   space-x-2 place-content-around";
                ];
              a_role [ "tooltip" ];
              a_aria "hidden" [];
              a_href "#";
            ]
          [
            span ~a:[ a_class [ "whitespace-nowrap" ] ] [ txt message ];
            information_icon;
            span
              ~a:
                [
                  a_class
                    [
                      "absolute hidden group-hover:flex -top-2 \
                       -translate-y-full w-48 px-2 py-1 bg-gray-700 rounded-lg \
                       text-center text-white text-sm";
                      down_arrow;
                    ];
                ]
              [ content ];
          ];
      ])

let speed_over_time speeds =
  assert (List.compare_length_with speeds 15 <= 0);
  let max_speed = List.fold_left Float.max 0.0 speeds in
  let green = `Color ("#32D583", None) in
  let red = `Color ("#F97066", None) in
  ignore red;
  Tyxml.Svg.(
    Tyxml.Html.svg
      ~a:[ a_fill `None; a_viewBox (0., 0., 165., 40.) ]
      (List.mapi
         (fun i speed ->
           let rel_speed = speed /. max_speed in
           let height = 40. *. rel_speed in
           rect
             ~a:
               [
                 a_x (15. *. float_of_int i, Some `Px);
                 a_y (40. -. height, Some `Px);
                 a_width (10., Some `Px);
                 a_height (height, Some `Px);
                 a_fill green;
               ]
             [])
         speeds))

let flash ?(status = "Info") flash_message =
  let flash_message_css_status =
    match status with
    | "Info" -> "flash-message--info"
    | "Success" -> "flash-message"
    | "Error" -> "flash-message--error"
    | _ -> "flash-message--info"
  in
  let close_button =
    Tyxml.Html.button
      ~a:
        [
          Tyxml.Html.a_class [ "icon-button" ];
          Tyxml_helpers.at_click "flashMessage=false";
        ]
      [
        Tyxml.Svg.(
          Tyxml.Html.svg
            ~a:
              [
                a_class [ "w-4 h-4" ];
                a_fill `None;
                a_viewBox (0., 0., 24., 24.);
                a_stroke `CurrentColor;
                a_stroke_width (2.5, Some `Px);
              ]
            [
              path
                ~a:
                  [
                    a_stroke_linecap `Round;
                    a_stroke_linejoin `Round;
                    a_d "M4.5 19.5l15-15m-15 0l15 15";
                  ]
                [];
            ]);
      ]
  in
  Tyxml.Html.(
    div
      ~a:
        [
          a_class [ "flash-message mt-8"; flash_message_css_status ];
          Tyxml_helpers.x_cloak;
          Tyxml_helpers.x_transition;
          Tyxml_helpers.x_data "{flashMessage: true}";
          Tyxml_helpers.x_show "flashMessage";
        ]
      [ txt flash_message; close_button ])

let flash_messages msgs =
  Tyxml.Html.div
    [ Tyxml.Html.div (List.map (fun (status, msg) -> flash ~status msg) msgs) ]

let breadcrumbs steps page_title =
  let separator =
    Tyxml.Svg.(
      Tyxml.Html.svg
        ~a:
          [
            a_class [ "h-4 w-5" ];
            a_fill `None;
            a_viewBox (0., 0., 24., 24.);
            a_stroke (`Color ("#D0D5DD", None));
            a_stroke_width (2.0, Some `Px);
          ]
        [
          path
            ~a:
              [
                a_stroke_linecap `Round;
                a_stroke_linejoin `Round;
                a_d "M9 5l7 7-7 7";
              ]
            [];
        ])
  in
  let add (prefix, results) (label, link) =
    let prefix = Fmt.str "%s/%s" prefix link in
    let link =
      Tyxml.Html.a ~a:[ Tyxml.Html.a_href prefix ] [ Tyxml.Html.txt label ]
    in
    (prefix, separator :: link :: results)
  in
  let _, steps = List.fold_left add ("", []) steps in
  let steps =
    Tyxml.Html.(
      a ~a:[ a_class [ "text-gray-700 dark:text-gray-500" ] ] [ txt page_title ])
    :: steps
  in
  Tyxml.Html.(
    div
      ~a:
        [
          a_class
            [
              "flex flex-wrap items-center mb-7 text-xs md:text-sm font-medium \
               space-x-0 md:space-x-2";
            ];
        ]
      (List.rev steps))

let table_head_div name =
  Tyxml.Html.(
    div
      ~a:
        [
          a_class
            [
              "bg-gray-50 dark:bg-gray-900 px-6 py-3 text-gray-500 \
               dark:text-gray-100 text-xs font-medium";
            ];
        ]
      [ txt name ])

let tabulate_div rows =
  Tyxml.Html.(
    div
      ~a:[ a_class [ "container-fluid mt-8 flex flex-col space-y-6" ] ]
      [
        div
          ~a:
            [
              a_class
                [
                  "bg-white dark:bg-gray-850 border border-gray-200 \
                   dark:border-gray-400 rounded-lg w-full overflow-hidden \
                   shadow-sm divide-y divide-gray-200";
                ];
            ]
          rows;
      ])

let truncate ~len s =
  let open Astring.String in
  let orig = length s in
  if len >= orig then s
  else
    let truncated = with_range ~len s in
    append truncated "…"

let duration (status : Build_status.t) t =
  let text =
    match status with
    | NotStarted -> "In queue for"
    | Failed -> "Failed in"
    | Passed -> "Passed in"
    | Pending -> "Running for"
    | Undefined _ -> "In queue for"
  in
  Printf.sprintf "%s %s" text (Run_time.Duration.pp_opt t)

let build_history_button history_url =
  Tyxml.Html.(
    a
      ~a:
        [
          a_class [ "btn btn-secondary btn-xs rounded-full" ];
          a_href history_url;
        ]
      [ txt "Build History" ])

let css =
  Printf.sprintf
    {|
@media (prefers-color-scheme: light) {
%s
}
@media (prefers-color-scheme: dark) {
%s
}
|}
    Ansi.css Ansi.css_dark

let logo_unsafe =
  (* FIXME: this an ugly trick to be able to use the dark mode in the ocaml logo *)
  Tyxml.Html.(
    Unsafe.data
      {|
<svg width="116" height="32" viewBox="0 0 116 32" fill="none" xmlns="http://www.w3.org/2000/svg">
<path d="M19.0675 28.1454C18.995 27.8545 19.14 27.4909 18.995 27.2C18.9225 26.9091 18.705 26.9091 18.6325 26.6909C18.3425 26.1091 17.98 25.4545 17.98 24.7273C17.98 24.1454 17.7625 23.5636 17.69 22.9818C17.69 22.6909 17.69 22.4 17.69 22.1091C17.69 21.9636 17.69 21.8182 17.6175 21.6727C17.6175 21.6 17.6175 21.4545 17.545 21.4545L17.6175 21.2364C17.6175 21.1636 18.27 21.1636 18.4875 21.1636C18.85 21.1636 19.14 21.1636 19.5025 21.2364C20.2275 21.2364 20.8075 21.2364 21.5325 21.1636C23.055 20.9454 23.7075 20.2909 24.07 20.0727C25.4475 19.1273 26.1 17.4545 26.1 17.4545C26.3175 16.9454 26.3175 16 26.825 15.6364C27.405 15.1273 28.3475 15.2 29 14.9091C29.3625 14.7636 29.6525 14.6182 30.0875 14.6909C30.3775 14.7636 30.885 15.1273 31.03 14.6182C30.9575 14.5454 30.885 14.4727 30.8125 14.4C31.3925 14.3273 30.8125 13.0909 30.595 12.8C30.2325 12.4364 29.725 12.2182 29.145 12.0727C28.42 11.8545 27.7675 11.6364 27.115 11.7818C25.955 12 26.0275 11.3454 25.375 11.3454C24.5775 11.3454 23.1275 11.4182 22.8375 12.2182C22.6925 12.5818 22.62 12.5818 22.4025 12.8727C22.2575 13.0909 22.4025 13.3091 22.33 13.6C22.2575 13.8909 22.1125 14.9091 21.9675 15.2727C21.75 15.8545 21.46 16.5818 20.9525 16.5818C20.2275 16.6545 19.6475 16.7273 19.0675 16.5091C18.705 16.3636 18.125 16.1454 17.835 16.0727C16.4575 15.4909 16.24 14.9091 16.24 14.9091C16.095 14.6909 15.7325 14.2545 15.5875 13.7454C15.4425 13.1636 15.1525 12.7273 15.08 12.4364C14.935 12.1454 14.7175 11.6364 14.5 11.1273C14.21 10.4727 13.8475 9.96363 13.5575 9.74545C13.1225 9.38181 12.76 8.79999 11.89 8.94545C11.745 8.94545 11.165 9.01817 10.73 9.38181C10.44 9.59999 10.3675 10.1091 10.0775 10.5454C9.93251 10.7636 9.64251 11.4909 9.42501 12.0727C9.28001 12.5091 9.20751 12.8 8.99001 12.9454C8.84501 13.0909 8.70001 13.2364 8.48251 13.1636C8.33751 13.0909 8.19251 13.0182 8.04751 12.9454C7.83001 12.8 7.39501 12 7.10501 11.4909C6.88751 10.9818 6.38001 10.2545 6.09001 9.8909C5.65501 9.30908 5.43751 9.16363 4.78501 9.16363C3.48001 9.16363 3.33501 9.8909 2.75501 10.9818C2.46501 11.4909 2.39251 12.2182 1.88501 12.8C1.66751 13.0909 0.72501 14.4727 0.0725098 14.6909V24.7273V24.6545C0.14501 24.5091 0.14501 24.3636 0.21751 24.2909C0.50751 23.7818 1.08751 23.2727 1.37751 22.6909C1.52251 22.4 1.74001 22.1091 1.88501 21.7454C1.95751 21.5273 2.03001 21.0909 2.17501 20.8727C2.32001 20.5818 2.61001 20.5091 2.90001 20.4364C3.40751 20.3636 3.77001 21.0909 4.35001 21.3818C4.56751 21.5273 5.72751 21.8909 6.09001 21.9636C6.67001 22.1091 7.25001 22.1818 7.83001 22.3273C8.12001 22.4 8.41001 22.4 8.77251 22.4727C9.06251 22.4727 10.15 22.5454 10.15 22.6182C9.64251 22.9091 9.28001 23.6364 9.06251 24.2182C8.84501 24.8 8.70001 25.4545 8.48251 25.9636C8.19251 26.5454 7.61251 26.8364 7.68501 27.5636C7.68501 27.8545 7.75751 28.1454 7.68501 28.4364C7.61251 28.8 7.46751 29.0182 7.39501 29.3818C7.25001 29.8182 7.10501 31.2 6.96001 31.5636L8.12001 31.4182C8.26501 31.1273 8.33751 29.8182 8.41001 29.6727C8.62751 29.0182 8.91751 28.5091 9.35251 28C9.78751 27.4909 9.78751 26.9091 10.005 26.3273C10.295 25.6727 10.6575 25.2364 11.02 24.6545C11.6725 23.6364 12.1075 22.2545 13.4125 21.9636C13.5575 21.9636 14.355 22.5454 14.7175 22.9091C15.1525 23.3454 15.5875 23.7818 15.8775 24.3636C16.385 25.4545 16.8925 27.0545 17.0375 27.8545C17.11 28.3636 17.1825 28.3636 17.545 28.8C17.69 28.9454 17.98 29.5273 18.125 29.7454C18.1975 29.9636 18.415 30.4727 18.4875 30.7636C18.56 30.9091 18.7775 31.4182 18.9225 31.8545H20.01C19.575 30.6182 19.2125 29.3818 19.0675 28.1454Z" fill="white"/>
<path d="M18.4875 30.6909C18.3425 30.4 18.1975 29.8909 18.125 29.6727C18.0525 29.4545 17.69 28.8727 17.545 28.7273C17.1825 28.3636 17.11 28.2909 17.0375 27.7818C16.8925 26.9091 16.385 25.3091 15.8775 24.2909C15.5875 23.7091 15.1525 23.2727 14.7175 22.8364C14.355 22.4727 13.5575 21.8909 13.4125 21.8909C12.035 22.1818 11.6725 23.4909 11.02 24.5818C10.6575 25.1636 10.295 25.6727 10.005 26.2545C9.715 26.8364 9.78749 27.4909 9.35249 27.9273C8.91749 28.4364 8.6275 28.9454 8.41 29.6C8.3375 29.7454 8.265 31.0545 8.12 31.3454L10.15 31.2C12.035 31.3454 11.455 32.0727 14.4275 31.9273L19.0675 31.7818C18.7775 31.3454 18.56 30.8364 18.4875 30.6909Z" fill="url(#paint0_linear_151_3935)"/>
<path d="M31.9725 0H4.64001C2.10251 0 0.0725098 2.10909 0.0725098 4.65455V14.7636C0.72501 14.4727 1.66751 13.0909 1.95751 12.7273C2.46501 12.1455 2.53751 11.3455 2.82751 10.9091C3.40751 9.81818 3.55251 9.09091 4.85751 9.09091C5.51001 9.09091 5.72751 9.23636 6.16251 9.81818C6.45251 10.1818 6.96001 10.9091 7.17751 11.3455C7.46751 11.9273 7.90251 12.6545 8.12001 12.8C8.26501 12.9455 8.41001 13.0182 8.55501 13.0182C8.77251 13.0909 8.91751 12.9455 9.06251 12.8C9.28001 12.6545 9.35251 12.3636 9.49751 11.9273C9.71501 11.3455 10.005 10.6182 10.15 10.4C10.44 9.96364 10.5125 9.45455 10.8025 9.23636C11.2375 8.87273 11.745 8.87273 11.9625 8.8C12.8325 8.65455 13.195 9.23636 13.63 9.6C13.92 9.81818 14.2825 10.3273 14.5725 10.9818C14.79 11.4909 15.08 12 15.1525 12.2909C15.225 12.5818 15.515 13.0182 15.66 13.6C15.805 14.1091 16.1675 14.4727 16.3125 14.7636C16.3125 14.7636 16.53 15.4182 17.9075 15.9273C18.1975 16.0727 18.7775 16.2182 19.14 16.3636C19.72 16.5818 20.3 16.5818 21.025 16.4364C21.5325 16.4364 21.8225 15.7091 22.04 15.1273C22.185 14.7636 22.33 13.7455 22.4025 13.4545C22.475 13.1636 22.2575 12.9455 22.475 12.7273C22.6925 12.4364 22.765 12.4364 22.91 12.0727C23.1275 11.2727 24.65 11.2 25.4475 11.2C26.1 11.2 26.0275 11.8545 27.1875 11.6364C27.84 11.4909 28.4925 11.7091 29.2175 11.9273C29.7975 12.0727 30.3775 12.2909 30.6675 12.6545C30.885 12.8727 31.3925 14.1818 30.885 14.2545C30.9575 14.3273 30.9575 14.4 31.1025 14.4727C30.9575 14.9818 30.45 14.6182 30.16 14.5455C29.7975 14.4727 29.5075 14.5455 29.0725 14.7636C28.42 15.0545 27.405 15.0545 26.8975 15.4909C26.39 15.9273 26.39 16.8 26.1725 17.3091C26.1725 17.3091 25.52 18.9818 24.1425 19.9273C23.78 20.2182 23.1275 20.8 21.605 21.0182C20.9525 21.0909 20.3 21.1636 19.575 21.0909C19.2125 21.0909 18.9225 21.0909 18.56 21.0182C18.3425 21.0182 17.69 21.0182 17.69 21.0909L17.6175 21.3091C17.6175 21.3818 17.6175 21.5273 17.69 21.5273C17.69 21.6727 17.69 21.8182 17.7625 21.9636C17.7625 22.2545 17.7625 22.5455 17.7625 22.8364C17.7625 23.4182 17.98 24 18.0525 24.5818C18.0525 25.2364 18.415 25.9636 18.705 26.5455C18.85 26.7636 18.995 26.7636 19.0675 27.0545C19.14 27.3455 19.0675 27.6364 19.14 28C19.285 29.2364 19.6475 30.4727 20.2275 31.6364C20.88 31.4909 21.605 31.2727 22.475 31.1273C24.07 30.9091 26.3175 30.9818 27.7675 30.9091C31.3925 30.5455 33.4225 32.4364 36.685 31.6364V4.65455C36.6125 2.10909 34.5825 0 31.9725 0Z" fill="url(#paint1_linear_151_3935)"/>
<path d="M8.48251 25.9636C8.77251 25.3818 8.91751 24.8 9.06251 24.2182C9.28001 23.6364 9.57001 22.9091 10.15 22.6182C10.0775 22.5454 8.99001 22.4727 8.77251 22.4727C8.48251 22.4727 8.19251 22.4 7.83001 22.3273C7.25001 22.1818 6.59751 22.1091 6.09001 21.9636C5.72751 21.8909 4.64001 21.4545 4.35001 21.3818C3.77001 21.0909 3.40751 20.3636 2.90001 20.4364C2.61001 20.5091 2.32001 20.5818 2.17501 20.8727C2.03001 21.0909 1.95751 21.5273 1.88501 21.8182C1.74001 22.1091 1.59501 22.4727 1.37751 22.7636C1.01501 23.3454 0.43501 23.8545 0.21751 24.3636C0.14501 24.5091 0.14501 24.5818 0.0725098 24.7273V30.9818C0.36251 31.0545 0.65251 31.1273 1.01501 31.2C3.55251 31.8545 4.20501 31.9273 6.74251 31.6364H6.96001C7.17751 31.2 7.32251 29.8909 7.39501 29.4545C7.46751 29.1636 7.61251 28.8727 7.68501 28.5091C7.75751 28.2182 7.68501 27.9273 7.68501 27.6364C7.61251 26.8364 8.19251 26.5454 8.48251 25.9636Z" fill="url(#paint2_linear_151_3935)"/>
<path class="svg-fill" d="M48.285 27.8545C47.0525 27.8545 45.965 27.6364 44.95 27.2C43.935 26.7636 43.065 26.1091 42.34 25.3091C41.615 24.5091 41.035 23.4909 40.6725 22.3273C40.31 21.1636 40.0925 19.8545 40.0925 18.3273C40.0925 16.8727 40.31 15.5636 40.6725 14.4C41.035 13.2364 41.615 12.2909 42.34 11.4909C43.065 10.6909 43.935 10.1091 44.95 9.67272C45.965 9.23636 47.0525 9.01817 48.285 9.01817C49.5175 9.01817 50.605 9.23636 51.62 9.67272C52.635 10.1091 53.505 10.6909 54.23 11.4909C54.955 12.2909 55.535 13.2364 55.8975 14.4C56.26 15.5636 56.4775 16.8727 56.4775 18.3273C56.4775 19.7818 56.26 21.0909 55.8975 22.3273C55.535 23.4909 54.955 24.5091 54.23 25.3091C53.505 26.1091 52.635 26.7636 51.62 27.2C50.605 27.6364 49.5175 27.8545 48.285 27.8545ZM48.285 24.3636C49.5175 24.3636 50.46 23.8545 51.185 22.7636C51.91 21.6727 52.2725 20.2909 52.2725 18.4C52.2725 16.5818 51.91 15.2 51.185 14.1818C50.46 13.1636 49.5175 12.6545 48.285 12.6545C47.0525 12.6545 46.11 13.1636 45.385 14.1818C44.66 15.2 44.2975 16.5818 44.2975 18.4C44.2975 20.2182 44.66 21.6727 45.385 22.7636C46.0375 23.8545 47.0525 24.3636 48.285 24.3636ZM57.855 18.6182C57.855 17.0909 58.0725 15.7818 58.58 14.6182C59.015 13.4545 59.6675 12.4364 60.465 11.6364C61.2625 10.8364 62.205 10.1818 63.22 9.74545C64.235 9.30908 65.395 9.0909 66.555 9.0909C67.6425 9.0909 68.73 9.30908 69.6 9.81817C70.5425 10.2545 71.2675 10.8364 71.8475 11.4909L69.6 14.0364C69.165 13.6 68.6575 13.3091 68.2225 13.0182C67.7875 12.8 67.2075 12.6545 66.555 12.6545C65.9025 12.6545 65.3225 12.8 64.815 13.0909C64.3075 13.3818 63.8 13.7454 63.365 14.2545C62.93 14.7636 62.64 15.3454 62.4225 16.0727C62.205 16.8 62.06 17.6 62.06 18.5454C62.06 20.4364 62.4225 21.8909 63.22 22.9091C64.0175 23.9273 65.105 24.4364 66.41 24.4364C67.135 24.4364 67.7875 24.2909 68.3675 24C68.9475 23.7091 69.3825 23.3454 69.89 22.8364L72.1375 25.3091C71.4125 26.1818 70.5425 26.8364 69.5275 27.2727C68.585 27.7091 67.4975 27.9273 66.41 27.9273C65.25 27.9273 64.1625 27.7091 63.1475 27.3454C62.1325 26.9818 61.19 26.3273 60.465 25.6C59.74 24.8727 59.0875 23.8545 58.6525 22.6909C58.0725 21.4545 57.855 20.1454 57.855 18.6182ZM73.4425 23.7091C73.4425 22.2545 74.0225 21.1636 75.255 20.3636C76.4875 19.5636 78.445 18.9818 81.1275 18.7636C81.055 18.1818 80.91 17.6727 80.5475 17.3091C80.185 16.9454 79.6775 16.8 78.9525 16.8C78.3725 16.8 77.7925 16.9454 77.2125 17.1636C76.6325 17.3818 75.98 17.6727 75.3275 18.1091L73.8775 15.4182C74.7475 14.9091 75.69 14.4727 76.6325 14.1091C77.575 13.7454 78.6625 13.6 79.6775 13.6C81.4175 13.6 82.795 14.1091 83.7375 15.1273C84.68 16.1454 85.115 17.6727 85.115 19.8545V27.7091H81.925L81.635 26.3273H81.5625C80.9825 26.8364 80.33 27.2727 79.6775 27.5636C79.025 27.8545 78.3 28.0727 77.5025 28.0727C76.85 28.0727 76.3425 28 75.835 27.7818C75.3275 27.5636 74.8925 27.2727 74.53 26.9091C74.1675 26.5454 73.8775 26.1091 73.7325 25.6C73.515 24.8727 73.4425 24.2909 73.4425 23.7091ZM77.285 23.4182C77.285 23.8545 77.43 24.2182 77.72 24.4364C78.01 24.6545 78.3725 24.7273 78.88 24.7273C79.3875 24.7273 79.75 24.6545 80.1125 24.4364C80.475 24.2182 80.765 23.9273 81.1275 23.5636V21.1636C79.6775 21.3818 78.6625 21.6727 78.0825 22.0364C77.575 22.4 77.285 22.9091 77.285 23.4182ZM87.58 13.8182H90.915L91.205 15.5636H91.35C91.93 14.9818 92.51 14.4727 93.1625 14.1091C93.815 13.6727 94.6125 13.4545 95.4825 13.4545C96.425 13.4545 97.2225 13.6727 97.875 14.0364C98.455 14.4 98.9625 14.9818 99.325 15.7091C99.905 15.0545 100.558 14.5454 101.21 14.1091C101.863 13.6727 102.66 13.4545 103.603 13.4545C105.053 13.4545 106.14 13.9636 106.865 14.9091C107.59 15.9273 107.88 17.2364 107.88 18.9818V27.4909H103.82V19.4909C103.82 18.4727 103.675 17.8182 103.385 17.4545C103.095 17.0909 102.66 16.8727 102.08 16.8727C101.428 16.8727 100.63 17.3091 99.76 18.1818V27.4909H95.7V19.4909C95.7 18.4727 95.555 17.8182 95.265 17.4545C94.975 17.0909 94.54 16.8727 93.96 16.8727C93.235 16.8727 92.51 17.3091 91.64 18.1818V27.4909H87.58V13.8182ZM110.2 8.14545H114.26V23.4909C114.26 23.9273 114.333 24.2182 114.478 24.3636C114.623 24.5091 114.768 24.5818 114.985 24.5818H115.203C115.275 24.5818 115.348 24.5818 115.493 24.5091L116 27.5636C115.783 27.6364 115.493 27.7091 115.13 27.7818C114.768 27.8545 114.405 27.8545 113.898 27.8545C113.173 27.8545 112.593 27.7091 112.158 27.4909C111.65 27.2727 111.288 26.9818 110.998 26.5454C110.708 26.1818 110.49 25.6727 110.345 25.0909C110.2 24.5818 110.2 24 110.2 23.2727V8.14545Z"/>
<defs>
<linearGradient id="paint0_linear_151_3935" x1="13.4756" y1="21.9126" x2="13.4756" y2="31.9457" gradientUnits="userSpaceOnUse">
<stop stop-color="#F29100"/>
<stop offset="1" stop-color="#EC670F"/>
</linearGradient>
<linearGradient id="paint1_linear_151_3935" x1="18.3366" y1="0.0149091" x2="18.3366" y2="31.9652" gradientUnits="userSpaceOnUse">
<stop stop-color="#F29100"/>
<stop offset="1" stop-color="#EC670F"/>
</linearGradient>
<linearGradient id="paint2_linear_151_3935" x1="5.10807" y1="20.4183" x2="5.10807" y2="31.7446" gradientUnits="userSpaceOnUse">
<stop stop-color="#F29100"/>
<stop offset="1" stop-color="#EC670F"/>
</linearGradient>
</defs>
</svg>|})
