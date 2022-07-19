open Tyxml.Html

let html_to_string = Fmt.to_to_string (Tyxml.Html.pp ())

let instance ?(flash_messages=[]) contents =
let flash_message_contents =
  ul (List.map (fun (category, text) ->
    li [ txt (Fmt.str "%s: %s" category text) ]) flash_messages)
  in
  html_to_string (
    html
      (head (title (txt "OCaml-CI")) [
          meta ~a:[a_charset "UTF-8"] ();
          link ~rel:[ `Stylesheet ] ~href:"/css/normalize.css" ();
          link ~rel:[ `Stylesheet ] ~href:"/css/ansi.css" ();
          link ~rel:[ `Stylesheet ] ~href:"/css/github.css" ();
          link ~rel:[ `Stylesheet ] ~href:"/css/style.css" ();
        ]
      )
      (body [
          nav [
            ul [
              li [a ~a:[a_href "/"] [txt "OCaml-CI"]];
            ]
          ];
          div [ flash_message_contents ];
          div ~a:[a_id "main"] contents
        ]
      )
  )
