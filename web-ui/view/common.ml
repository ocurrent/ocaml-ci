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
    ~action:(variant ^ "/rebuild") ~input_value:"Rebuild"

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
          a_class [ "h-5 w-5 -rotate-90" ];
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

let github_logo =
  Tyxml.Svg.(
    Tyxml.Html.svg
      ~a:[ a_class [ "w-4 h-4" ]; a_fill `None; a_viewBox (0., 0., 14., 14.) ]
      [
        path
          ~a:
            [
              a_fill (`Color ("black", None));
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
    Tyxml.Html.(a ~a:[ a_class [ "text-gray-700" ] ] [ txt page_title ])
    :: steps
  in
  Tyxml.Html.(
    div
      ~a:[ a_class [ "flex items-center mb-7 text-sm font-medium space-x-2" ] ]
      (List.rev steps))

let table_head name =
  Tyxml.Html.(
    div
      ~a:
        [ a_class [ "bg-gray-50 px-6 py-3 text-gray-500 text-xs font-medium" ] ]
      [ txt name ])

let tabulate rows =
  Tyxml.Html.(
    div
      ~a:[ a_class [ "container-fluid mt-8 flex flex-col space-y-6" ] ]
      [
        div
          ~a:
            [
              a_class
                [
                  "border border-gray-200 rounded-lg w-full overflow-hidden \
                   shadow-sm  divide-y divide-gray-200";
                ];
            ]
          rows;
      ])
