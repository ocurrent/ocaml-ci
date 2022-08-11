open Tyxml.Html

let html_to_string = Fmt.to_to_string (pp ())

let instance ?(flash_messages = []) contents =
  let flash_div (category, text) =
    let close_x = "\u{2715}" in
    div
      ~a:[ a_class [ "bar"; "info" ] ]
      [
        div
          ~a:[ a_class [ "close" ]; a_onclick "this.parentElement.remove()" ]
          [ txt close_x ];
        txt (Fmt.str "%s: %s" category text);
      ]
  in
  html_to_string
    (html
       (head
          (title (txt "OCaml-CI"))
          [
            meta ~a:[ a_charset "UTF-8" ] ();
            link ~rel:[ `Stylesheet ] ~href:"/css/normalize.css" ();
            link ~rel:[ `Stylesheet ] ~href:"/css/ansi.css" ();
            link ~rel:[ `Stylesheet ] ~href:"/css/github.css" ();
            link ~rel:[ `Stylesheet ] ~href:"/css/style.css" ();
            link ~rel:[ `Stylesheet ] ~href:"/css/flash-messages.css" ();
          ])
       (body
          [
            nav [ ul [ li [ a ~a:[ a_href "/" ] [ txt "OCaml-CI" ] ] ] ];
            div (List.map flash_div flash_messages);
            div ~a:[ a_id "main" ] contents;
          ]))
