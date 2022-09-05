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
  Tyxml.Html.(div ~a:[ a_class [ "icon-status icon-status--active" ] ] [])

let icon_queued =
  Tyxml.Html.(div ~a:[ a_class [ "icon-status icon-status--default" ] ] [])

let status_icon (status : Ocaml_ci_api.Client.State.t) =
  match status with
  | NotStarted -> icon_queued
  | Failed _ | Aborted -> icon_failed
  | Passed -> icon_success
  | Active -> icon_active
  | Undefined _ -> icon_failed

let rebuild_button =
  [
    Tyxml.Html.(
      button
        ~a:
          [
            a_class [ "btn btn-primary" ];
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
        [ button [ txt "Rebuild all" ]; button [ txt "Rebuild failed" ] ]);
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

let flash ?(status = "info") flash_message =
  let flash_message_css_status =
    match status with
    | "info" -> "flash-message--info"
    | "success" -> "flash-message"
    | "error" -> "flash-message--error"
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
