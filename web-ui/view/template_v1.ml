open Tyxml.Html

let html_to_string = Fmt.to_to_string (pp ())

let head =
  head
    (title (txt "OCaml-CI"))
    [
      meta ~a:[ a_charset "UTF-8" ] ();
      meta ~a:[ a_http_equiv "X-UA-Compatible"; a_content "IE=edge" ] ();
      meta
        ~a:
          [
            a_name "viewport"; a_content "width=device-width, initial-scale=1.0";
          ]
        ();
      script ~a:[ a_defer (); a_src "/js/alpine.js" ] (txt "");
      script ~a:[ a_defer (); a_src "/js/alpine-clipboard.js" ] (txt "");
      link ~rel:[ `Stylesheet ] ~href:"/fonts/inter.css" ();
      link ~rel:[ `Stylesheet ] ~href:"/css/main.css" ();
      link ~rel:[ `Stylesheet ] ~href:"/css/ansi.css" ();
    ]

let header ~full =
  let constrained = "container-fluid flex space-x-6 items-end" in
  let maximised = "px-12 flex space-x-6 items-end" in
  let klass = if full then maximised else constrained in
  header
    ~a:
      [
        a_class
          [
            "h-20 flex items-center border-b border-gray-200 bg-white top-0 \
             z-50";
          ];
      ]
    [
      div
        ~a:[ a_class [ klass ] ]
        [
          a
            ~a:[ a_href "/" ]
            [
              img ~src:"/images/logo.svg" ~alt:"OCaml-CI"
                ~a:[ a_width 116; a_height 32 ]
                ();
            ];
          div
            ~a:[ a_class [ "space-x-4" ] ]
            [
              a
                ~a:
                  [
                    a_class [ "header-link link-hover" ];
                    a_href "/getting-started";
                  ]
                [ txt "Getting Started" ];
              a
                ~a:[ a_class [ "header-link link-hover" ]; a_href "#" ]
                [ txt "Documentation" ];
            ];
        ];
    ]

let instance ?(full = false) contents =
  let constrained = "container-fluid py-12" in
  let maximised = "flex-basis px-12 py-12" in
  let klass = if full then maximised else constrained in

  html_to_string
    (html head
       (body
          ~a:[ a_class [ "" ] ]
          [ header ~full; div ~a:[ a_class [ klass ] ] contents ]))
