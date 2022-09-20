let title_card ~status ~card_title ~hash_link ~created_at ~finished_at
    ~queued_for ~ran_for ~button =
  let rebuild_button = Option.value ~default:(Tyxml.Html.div []) button in
  Tyxml.Html.(
    div
      ~a:[ a_class [ "justify-between items-center flex" ] ]
      [
        div
          ~a:[ a_class [ "flex flex-col space-y-6" ] ]
          [
            div
              ~a:[ a_class [ "flex items-center space-x-4" ] ]
              [
                Common.status_icon status;
                div
                  ~a:[ a_class [ "flex flex-col space-y-1" ] ]
                  [
                    div
                      ~a:[ a_class [ "flex items-baseline space-x-2" ] ]
                      [
                        h1 ~a:[ a_class [ "text-xl" ] ] [ txt card_title ];
                        (* TODO: Breakdown by OS, Compiler and Opam
                           <div class="text-sm font-normal text-gray-500">
                             OS: debian-11 - Compiler: 4.14+flambda - Opam: 2.1
                           </div>
                        *)
                      ];
                    div
                      ~a:[ a_class [ "text-gray-500" ] ]
                      [
                        div
                          ~a:[ a_class [ "flex text-sm space-x-2" ] ]
                          [
                            div [ txt @@ Fmt.str "Created at: %s" created_at ];
                            div [ txt "-" ];
                            div [ txt @@ Fmt.str "%s in queue" queued_for ];
                            div [ txt "-" ];
                            div [ txt @@ Fmt.str "Finished at: %s" finished_at ];
                            div [ txt "-" ];
                            div [ hash_link ];
                          ];
                      ];
                  ];
              ];
          ];
        div
          ~a:[ a_class [ "flex items-center justify-between space-x-4" ] ]
          [
            div
              ~a:[ a_class [ "text-sm" ] ]
              [ txt @@ Fmt.str "Ran for %s" ran_for ];
            rebuild_button;
          ];
      ])
