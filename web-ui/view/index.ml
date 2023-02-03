open Tyxml.Html

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

let search_bar_options = function
  | [] | [ _ ] -> []
  | orgs ->
      let build_option =
        option ~a:[ a_value "all" ] (txt "All")
        :: List.map
             (fun (prefix, name, _) -> option ~a:[ a_value prefix ] (txt name))
             orgs
      in
      [
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
              build_option;
          ];
      ]

let list_orgs ~orgs =
  let org_rows =
    let generate (prefix, _, orgs) = rows prefix orgs in
    List.concat_map generate orgs
  in
  Template.instance
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
            ([
               div
                 ~a:[ a_class [ "form-control max-w-80 pb-6 md:pb-0" ] ]
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
             ]
            @ search_bar_options orgs);
        ];
      div
        ~a:[ a_id "table"; a_class [ "mt-8 grid gap-x-4 md:grid-cols-2" ] ]
        org_rows;
    ]
