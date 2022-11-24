open Tyxml.Html

let head =
  head
    (title (txt "OCaml-CI - Error"))
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
        ~a:[ a_class [ "container-fluid flex justify-between items-center" ] ]
        [
          a
            ~a:[ a_href "/" ]
            [
              img ~src:"/images/logo.svg" ~alt:"OCaml-CI"
                ~a:[ a_width 116; a_height 32 ]
                ();
            ];
        ];
    ]

let body ~code ~reason =
  body
    ~a:[ a_class [ "" ] ]
    [
      header;
      div
        ~a:[ a_class [ "container-fluid py-12 text-center" ] ]
        [
          div
            ~a:
              [
                a_class
                  [
                    "flex justify-center flex-col items-center fixed left-0 \
                     top-0 w-full h-full px-10";
                  ];
              ]
            [
              h1 ~a:[ a_class [ "text-stroke" ] ] [ txt (Fmt.str "%d" code) ];
              div
                ~a:[ a_class [ "text-2xl font-medium my-6" ] ]
                [ txt (Fmt.str "Oops! %s" reason) ];
              div
                [
                  txt
                    "We're sorry, the page you requested could not be found. \
                     Please go back to the homepage.";
                ];
              a
                ~a:[ a_class [ "btn btn-secondary mt-8 btn-lg" ]; a_href "/" ]
                [ txt "Take me home" ];
            ];
        ];
    ]

(* https://github.com/aantron/dream/tree/master/example/9-error#9-error *)
let ocaml_ci_error_template _error _debug_info suggested_response =
  let status = Dream.status suggested_response in
  let code = Dream.status_to_int status
  and reason = Dream.status_to_string status in
  let html = Fmt.to_to_string (pp ()) (html head (body ~code ~reason)) in
  Dream.set_header suggested_response "Content-Type" Dream.text_html;
  Dream.set_body suggested_response html;
  Lwt.return suggested_response
