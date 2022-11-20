module Client = Ocaml_ci_api.Client
open Tyxml.Html
open Git_forge

module Make (M : M_Git_forge) = struct
  type t = Branch of string | PR of { title : string; id : string }

  let logo =
    match M.prefix with
    | "github" -> Common.github_logo
    | "gitlab" -> Common.gitlab_logo
    | _ -> raise Not_found

  let row ~ref ~short_hash ~started_at ~ran_for ~status ~ref_uri ~message =
    ignore started_at;
    (*See FIXME - revert this when started_at is implemented *)
    (* messages are of arbitrary length - let's truncate them *)
    let message = Astring.String.with_range ~len:72 message in
    let ref_title =
      match ref with Branch title -> title | PR { title; _ } -> title
    in
    let description =
      [ div [ txt short_hash ] ]
      @ (match ref with
        | Branch _ -> []
        | PR { id; _ } ->
            [
              div [ txt "-" ];
              div
                ~a:[ a_class [ "flex space-x-1 items-center" ] ]
                [ logo; div [ txt (Printf.sprintf "#%s" id) ] ];
            ])
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
            div [ txt (Timestamps_durations.pp_timestamp ran_for) ];
            Common.right_arrow_head;
          ]
    in
    a
      ~a:[ a_class [ "table-row" ]; a_href ref_uri ]
      [
        div
          ~a:[ a_class [ "flex items-center space-x-3" ] ]
          [
            Common.status_icon_build status;
            div
              ~a:[ a_class [ "flex items-center space-x-3" ] ]
              [
                div
                  ~a:
                    [
                      a_class
                        [
                          "font-medium text-gray-700 text-sm px-2 py-1 border \
                           border-gray-300 rounded-lg";
                        ];
                    ]
                  [ txt ref_title ];
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

  let ref gref title =
    match Astring.String.cuts ~sep:"/" gref with
    | "refs" :: "heads" :: branch ->
        Branch (Astring.String.concat ~sep:"/" branch)
    | [ "refs"; "pull"; id; "head" ] -> PR { title; id }
    | _ -> Branch (Printf.sprintf "Bad ref format %S" gref)

  let list ~org ~repo ~default_ref ~refs =
    let f { Client.Repo.gref; hash; status; started_at; message; name; ran_for }
        =
      let short_hash = short_hash hash in
      row ~ref:(ref gref name) ~short_hash ~started_at ~ran_for ~status
        ~ref_uri:(Url.commit_url M.prefix ~org ~repo ~hash)
        ~message
    in
    let default_table, main_ref =
      let main_ref, main_ref_info =
        Client.Ref_map.bindings refs
        |> List.find (fun (_, { Client.Repo.name; _ }) ->
               String.equal name default_ref)
      in
      let table_head = Common.table_head_div "Default Branch" in
      let table = table_head :: [ f main_ref_info ] in
      (table, main_ref)
    in
    let refs = Client.Ref_map.remove main_ref refs in
    let branch_table, n_branches =
      let branches =
        Client.Ref_map.filter
          (fun ref _ -> String.starts_with ~prefix:"refs/heads/" ref)
          refs
      in
      let n_branches = Client.Ref_map.cardinal branches in
      let table_head =
        Common.table_head_div (Printf.sprintf "Branches (%d)" n_branches)
      in
      let bindings = Client.Ref_map.bindings branches in
      let table = table_head :: List.map (fun (_, ref) -> f ref) bindings in
      (table, n_branches)
    in
    let pr_table, n_prs =
      let prs =
        Client.Ref_map.filter
          (fun ref _ -> String.starts_with ~prefix:"refs/pull/" ref)
          refs
      in
      let n_prs = Client.Ref_map.cardinal prs in
      let table_head =
        Common.table_head_div (Printf.sprintf "Refs Branches (%d)" n_prs)
      in
      let bindings = Client.Ref_map.bindings prs in
      let table = table_head :: List.map (fun (_, ref) -> f ref) bindings in
      (table, n_prs)
    in
    let title =
      let repo_url = Url.repo_url M.prefix ~org ~repo in
      div
        ~a:[ a_class [ "justify-between items-center flex" ] ]
        [
          div
            ~a:[ a_class [ "flex items-center space-x-2" ] ]
            [
              div
                ~a:[ a_class [ "flex flex-col space-y-1" ] ]
                [
                  div
                    ~a:[ a_class [ "flex text-sm space-x-2 items-baseline" ] ]
                    [
                      h1 ~a:[ a_class [ "text-xl" ] ] [ txt repo ];
                      a
                        ~a:
                          [
                            a_class [ "flex items-center space-x-2" ];
                            a_href repo_url;
                          ]
                        [ span [ txt repo_url ]; Common.external_link ];
                    ];
                ];
            ];
        ]
    in
    [
      Common.breadcrumbs [ (M.prefix, M.prefix); (org, org) ] repo;
      title;
      Common.tabulate_div default_table;
    ]
    |> (fun content ->
         if n_branches = 0 then content
         else content @ [ Common.tabulate_div branch_table ])
    |> (fun content ->
         if n_prs = 0 then content
         else content @ [ Common.tabulate_div pr_table ])
    |> Template_v1.instance
end
