open Tyxml.Html

let render github gitlab =
  Template.instance
    [
      p [ txt "Welcome to OCaml-CI!" ];
      p
        [
          txt "See ";
          a
            ~a:[ a_href "https://github.com/apps/ocaml-ci" ]
            [ txt "The OCaml-CI GitHub App" ];
          txt " for details.";
        ];
      ul
        [
          (if Option.is_some github then
           li
             [
               a
                 ~a:[ a_href "/github" ]
                 [ txt "Registered GitHub organisations" ];
             ]
          else li [ txt "No GitHub organisations" ]);
          (if Option.is_some gitlab then
           li
             [
               a
                 ~a:[ a_href "/gitlab" ]
                 [ txt "Registered GitLab organisations" ];
             ]
          else li [ txt "No GitLab organisations" ]);
        ];
    ]

let rows prefix orgs =
  let open Organisation.Make (struct
    let prefix = prefix
  end) in
  let compare s0 s1 =
    String.(
      compare
        (lowercase_ascii s0.Client.CI.name)
        (lowercase_ascii s1.Client.CI.name))
  in
  let orgs = List.sort compare orgs in
  List.fold_left (fun l org -> List.append l [ row ~org ]) [] orgs

let list_orgs prefix orgs =
  Template_v1.instance
    [
      Tyxml.Html.script ~a:[ a_src "/js/index-page-org-search.js" ] (txt "");
      div
        ~a:
          [
            a_class [ "flex flex-col md:flex-row justify-between items-center" ];
          ]
        [
          div
            ~a:
              [
                a_class
                  [
                    "flex flex-col space-y-1 items-center md:items-start py-8 \
                     md:py-0";
                  ];
              ]
            [
              h1 [ txt "Welcome to OCaml-CI" ];
              div
                ~a:[ a_class [ "text-gray-500" ] ]
                [ txt "Here are the organisations registered with us" ];
            ];
          div
            ~a:[ a_class [ "form-control relative max-w-80" ] ]
            [
              Common.search;
              input
                ~a:
                  [
                    a_input_type `Text;
                    a_placeholder "Search for an organisation";
                    a_oninput "search(this.value)";
                  ]
                ();
            ];
        ];
      div
        ~a:[ a_class [ "mt-8 md:columns-2" ] ]
        [
          div
            ~a:[ a_id "table"; a_class [ "flex-col space-y-6" ] ]
            (rows prefix orgs);
        ];
    ]

(** TODO: this function can be factorized with the one above. *)
let list_all_orgs ~github_orgs ~gitlab_orgs =
  let github_org_rows = rows "github" github_orgs in
  let gitlab_org_rows = rows "gitlab" gitlab_orgs in
  let org_rows = github_org_rows @ gitlab_org_rows in

  Template_v1.instance
    [
      Tyxml.Html.script ~a:[ a_src "/js/index-page-org-search.js" ] (txt "");
      div
        ~a:
          [
            a_class [ "flex flex-col md:flex-row justify-between items-center" ];
          ]
        [
          div
            ~a:
              [
                a_class
                  [
                    "flex flex-col space-y-1 items-center md:items-start py-8 \
                     md:py-0";
                  ];
              ]
            [
              h1 [ txt "Welcome to OCaml-CI" ];
              div
                ~a:[ a_class [ "text-gray-500 dark:text-gray-400" ] ]
                [ txt "Here are the organisations registered with us" ];
            ];
          div
            ~a:
              [
                a_class
                  [
                    "flex flex-col md:flex-row items-center justify-between \
                     space-x-3";
                  ];
              ]
            [
              div
                ~a:[ a_class [ "form-control relative max-w-80 pb-6 md:pb-0" ] ]
                [
                  Common.search;
                  input
                    ~a:
                      [
                        a_input_type `Text;
                        a_placeholder "Search for an organisation";
                        a_oninput "search(this.value)";
                      ]
                    ();
                ];
              div
                ~a:[ a_class [ "relative" ] ]
                [
                  select
                    ~a:
                      [
                        a_class
                          [
                            "input-control relative input-text text-gray-500 \
                             dark:text-gray-300 bg-gray-100 dark:bg-gray-850 \
                             items-center justify-between flex px-3 py-2 \
                             appearance-none";
                          ];
                        a_name "Languages";
                        a_onchange "filter(this.value)";
                      ]
                    [
                      option ~a:[ a_value "all" ] (txt "All");
                      option ~a:[ a_value "gitlab" ] (txt "Gitlab");
                      option ~a:[ a_value "github" ] (txt "GitHub");
                    ];
                ];
            ];
        ];
      div
        ~a:[ a_class [ "mt-8 md:columns-2" ] ]
        [ div ~a:[ a_id "table"; a_class [ "flex-col space-y-6" ] ] org_rows ];
    ]
