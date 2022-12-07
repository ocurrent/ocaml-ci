open Tyxml.Html

module Make (M : Git_forge_intf.Forge) = struct
  let row ~short_hash ~started_at ~ran_for ~status ~build_uri ~message =
    let message = Common.truncate ~len:72 message in
    let description =
      [ div [ txt short_hash ] ]
      @
      match started_at with
      | None -> []
      | Some _ ->
          [
            div [ txt "-" ];
            div [ txt (Timestamps_durations.pp_timestamp started_at) ];
          ]
    in
    let rhs =
      match ran_for with
      | None -> [ Common.right_arrow_head ]
      | Some _ ->
          [
            div [ txt (Common.duration status ran_for) ];
            Common.right_arrow_head;
          ]
    in
    a
      ~a:[ a_class [ "table-row" ]; a_href build_uri ]
      [
        div
          ~a:[ a_class [ "flex items-center space-x-3" ] ]
          [
            Common.status_icon_build status;
            div
              ~a:[ a_class [ "flex items-center space-x-3" ] ]
              [
                div
                  ~a:[ a_class [ "flex flex-col" ] ]
                  [
                    div
                      ~a:[ a_class [ "text-gray-900 text-sm font-medium" ] ]
                      [ txt message ];
                    div ~a:[ a_class [ "flex text-sm space-x-2" ] ] description;
                  ];
              ];
          ];
        div
          ~a:
            [
              a_class
                [
                  "flex text-sm font-normal text-gray-500 space-x-8 \
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

  let top_matter ~org ~repo ~ref ~tref =
    let external_url = M.branch_url ~org ~repo ref in
    div
      ~a:[ a_class [ "justify-between items-center flex" ] ]
      [
        div
          ~a:[ a_class [ "flex items-center grow" ] ]
          [
            div
              ~a:[ a_class [ "flex flex-col space-y-1" ] ]
              [
                h1
                  ~a:[ a_class [ "text-xl" ] ]
                  [ span ~a:[a_class ["flex grow"]]
                    [txt (Printf.sprintf "Build History for %s" tref);
                    a ~a:[ a_class ["ml-2"]; a_href external_url ] [ Common.external_link ]
                    ]];

                div
                  ~a:[ a_class [ "text-gray-500 flex text-sm space-x-2" ] ]
                  [
                    span
                      [
                        txt
                          (Printf.sprintf "Repo: %s" repo);
                      ];
                  ];
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

  let list ~org ~repo ~ref ~history =
    let tref = Result.get_ok @@ M.ref_path ref in
    Template_v1.instance
      [
        Common.breadcrumbs
          [ (M.prefix, M.prefix); (org, org); (repo, repo); (tref, tref) ]
          "Build History";
        top_matter ~org ~repo ~ref ~tref;
        Common.tabulate_div @@ history_v ~org ~repo ~history;
      ]
end
