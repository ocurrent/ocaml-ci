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
      script
        ~a:
          [
            a_defer (); a_src "https://unpkg.com/alpinejs@3.x.x/dist/cdn.min.js";
          ]
        (txt "");
      link ~rel:[ `Stylesheet ]
        ~href:
          "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"
        ();
      link ~rel:[ `Stylesheet ] ~href:"/css/tailwind.css" ();
    ]

let header =
  header
    ~a:
      [
        a_class
          [
            "h-20 flex items-center border-b border-gray-200 bg-white sticky \
             top-0 z-50";
          ];
      ]
    [
      div
        ~a:[ a_class [ "container-fluid flex space-x-6 items-end" ] ]
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
                ~a:[ a_class [ "header-link link-hover" ]; a_href "#" ]
                [ txt "Getting Started" ];
              a
                ~a:[ a_class [ "header-link link-hover" ]; a_href "#" ]
                [ txt "Documentation" ];
            ];
        ];
    ]

let instance contents =
  html_to_string
    (html head
       (body
          ~a:[ a_class [ "" ] ]
          [ header; div ~a:[ a_class [ "container-fluid"; "py-12" ] ] contents ]))
