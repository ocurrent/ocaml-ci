open Tyxml.Html

module Make (M : Git_forge_intf.Forge) = struct
  let row ~short_hash ~started_at ~ran_for ~status ~build_uri ~message =
    let description =
      [ div [ txt short_hash ] ]
      @
      match started_at with
      | None -> []
      | Some _ ->
          [
            div ~a:[ a_class [ "hidden md:inline" ] ] [ txt "-" ];
            div
              ~a:[ a_class [ "hidden md:inline" ] ]
              [ txt (Timestamps_durations.pp_timestamp started_at) ];
          ]
    in
    let rhs =
      match ran_for with
      | None -> [ Common.right_arrow_head ]
      | Some _ ->
          [
            div
              ~a:[ a_class [ "hidden md:inline" ] ]
              [ txt (Common.duration status ran_for) ];
            Common.right_arrow_head;
          ]
    in
    a
      ~a:[ a_class [ "table-row space-x-3" ]; a_href build_uri ]
      [
        div
          ~a:[ a_class [ "flex items-center space-x-3 truncate" ] ]
          [
            Common.status_icon_build status;
            div
              ~a:[ a_class [ "flex items-center space-x-3 grow-0 truncate" ] ]
              [
                div
                  ~a:[ a_class [ "flex flex-col truncate" ] ]
                  [
                    div
                      ~a:
                        [
                          a_class
                            [
                              "text-gray-900 dark:text-gray-100 text-sm \
                               font-medium truncate";
                            ];
                        ]
                      [ txt message ];
                    div
                      ~a:[ a_class [ "flex text-sm text-gray-500 space-x-2" ] ]
                      description;
                  ];
              ];
          ];
        div
          ~a:
            [
              a_class
                [
                  "hidden md:flex text-sm font-normal text-gray-500 space-x-8 \
                   items-center";
                ];
            ]
          rhs;
      ]

  let history_v ~org ~repo ~(history : Git_forge_intf.Client.Repo.ref_info list)
      =
    let n_builds = List.length history in
    let f
        {
          Git_forge_intf.Client.Repo.gref;
          hash;
          status;
          started_at;
          message;
          name;
          ran_for;
        } =
      ignore name;
      ignore gref;
      let short_hash = Common.short_hash hash in
      row ~short_hash ~started_at ~ran_for ~status
        ~build_uri:(Url.commit_url M.prefix ~org ~repo ~hash)
        ~message
    in
    let table_head =
      Common.table_head_div (Printf.sprintf "Builds (%d)" n_builds)
    in
    table_head :: List.map f history

  let top_matter ~org ~repo ~gref ~tref =
    let external_url =
      match gref with
      | `Branch branch -> M.branch_url ~org ~repo branch
      | `Request id ->
          let id = string_of_int id in
          M.request_url ~org ~repo id
    in
    div
      ~a:[ a_class [ "justify-between items-center flex space-x-4" ] ]
      [
        div
          ~a:[ a_class [ "flex items-center grow" ] ]
          [
            div
              ~a:[ a_class [ "flex flex-col space-y-1" ] ]
              [
                h1
                  ~a:[ a_class [ "text-xl" ] ]
                  [
                    span
                      ~a:[ a_class [ "flex grow" ] ]
                      [
                        txt (Printf.sprintf "Build History for %s" tref);
                        a
                          ~a:
                            [
                              a_class [ "hidden md:inline ml-2" ];
                              a_href external_url;
                            ]
                          [ Common.external_link ];
                      ];
                  ];
                (* div
                   ~a:[ a_class [ "text-gray-500 flex text-sm space-x-2" ] ]
                   [ span [ txt (Printf.sprintf "Repo: %s" repo) ] ]; *)
              ];
          ];
        div
          ~a:
            [ a_class [ "form-control relative w-80" ]; a_style "display:none" ]
          [
            Common.search;
            input
              ~a:
                [
                  a_input_type `Text;
                  a_placeholder "Search for a branch";
                  a_oninput "search(this.value)";
                ]
              ();
          ];
      ]

  let list ~org ~repo ~gref ~head_commit ~history =
    let tref =
      match gref with
      | `Branch branch -> Printf.sprintf "branch/%s" branch
      | `Request id -> Printf.sprintf "%s/%d" M.request_prefix id
    in
    let breadcrumbs =
      match head_commit with
      | None -> [ (M.prefix, M.prefix); (org, org); (repo, repo) ]
      | Some commit ->
          [
            (M.prefix, M.prefix);
            (org, org);
            (repo, repo);
            (tref, Printf.sprintf "commit/%s" commit);
          ]
    in
    Template.instance
      [
        Common.breadcrumbs breadcrumbs "Build History";
        top_matter ~org ~repo ~gref ~tref;
        Common.tabulate_div @@ history_v ~org ~repo ~history;
      ]
end
