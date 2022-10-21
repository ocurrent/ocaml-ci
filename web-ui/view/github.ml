module Client = Ocaml_ci_api.Client
module StatusTree = Status_tree
module Build_status = Build_status
module Run_time = Ocaml_ci_client_lib.Run_time
open Tyxml.Html
open Git_forge

(* Paths for HTML links *)
let prefix = "github"
let org_url org = Printf.sprintf "/%s/%s" prefix org
let repo_url org repo = Printf.sprintf "/%s/%s/%s" prefix org repo

let commit_url ~org ~repo hash =
  Printf.sprintf "/%s/%s/%s/commit/%s" prefix org repo hash

let job_url ~org ~repo ~hash variant =
  Printf.sprintf "/%s/%s/%s/commit/%s/variant/%s" prefix org repo hash variant

let github_branch_url ~org ~repo ref =
  Printf.sprintf "https://github.com/%s/%s/tree/%s" org repo ref

let github_commit_url ~org ~repo ~hash =
  Printf.sprintf "https://github.com/%s/%s/commit/%s" org repo hash

let github_pr_url ~org ~repo id =
  Printf.sprintf "https://github.com/%s/%s/pull/%s" org repo id

let github_org_url org = Printf.sprintf "https://github.com/%s" org

let github_repo_url ~org repo =
  Printf.sprintf "https://github.com/%s/%s" org repo

let ref_name r =
  match Astring.String.cuts ~sep:"/" r with
  | "refs" :: "heads" :: branch -> Astring.String.concat ~sep:"/" branch
  | [ "refs"; "pull"; id; "head" ] -> id
  | _ -> Fmt.str "Bad ref format %S" r

let ref_breadcrumb r head_hash =
  match Astring.String.cuts ~sep:"/" r with
  | "refs" :: "heads" :: branch ->
      let branch = Astring.String.concat ~sep:"/" branch in
      (branch, Printf.sprintf "commit/%s" head_hash)
  | [ "refs"; "pull"; id; "head" ] ->
      ("#" ^ id, Printf.sprintf "pull/%s" head_hash)
  | _ -> (Fmt.str "Bad ref format %S" r, "")

let link_github_commit ~org ~repo ~hash =
  a ~a:[ a_href (github_commit_url ~org ~repo ~hash) ] [ txt hash ]

let link_github_refs ~org ~repo refs =
  let f r =
    match Astring.String.cuts ~sep:"/" r with
    | "refs" :: "heads" :: branch ->
        let branch = Astring.String.concat ~sep:"/" branch in
        a ~a:[ a_href (github_branch_url ~org ~repo branch) ] [ txt branch ]
    | [ "refs"; "pull"; id; "head" ] ->
        a ~a:[ a_href (github_pr_url ~org ~repo id) ] [ txt ("PR#" ^ id) ]
    | _ -> txt ""
  in
  List.map f refs

let list_orgs ~orgs =
  let org_table =
    let f { Client.CI.owner; description = _description; n_repos } =
      a
        ~a:[ a_href (org_url owner); a_class [ "item-card flex space-x-4" ] ]
        [
          (* Github sometimes returns a blurry smaller profile picture,
             so request larger than we need and downsample *)
          img
            ~a:[ a_style "border-radius: 50%; width: 88px" ]
            ~src:(Printf.sprintf "https://github.com/%s.png?size=200" owner)
            ~alt:(Printf.sprintf "%s profile picture" owner)
            ();
          div
            ~a:[ a_class [ "flex flex-col" ] ]
            [
              div ~a:[ a_class [ "font-semibold text-lg mb-1" ] ] [ txt owner ];
              (* FIXME [benmandrew]: [description] here, currently only placeholder exists *)
              div ~a:[ a_class [ "text-sm" ] ] [ txt "" ];
              div
                ~a:
                  [
                    a_class
                      [
                        "flex mt-4 text-sm text-gray-700 font-normal space-x-4";
                      ];
                  ]
                [
                  div [ txt (github_org_url owner) ];
                  div
                    ~a:[ a_class [ "flex items-center space-x-2" ] ]
                    [ txt (Printf.sprintf "%d repositories" n_repos) ];
                ];
            ];
        ]
    in
    List.map f orgs
  in
  let title =
    div
      ~a:[ a_class [ "justify-between items-center flex" ] ]
      [
        div
          ~a:[ a_class [ "flex flex-col space-y-1" ] ]
          [
            h1 ~a:[ a_class [ "text-xl" ] ] [ txt "Welcome to Ocaml-ci" ];
            div
              ~a:[ a_class [ "text-gray-500" ] ]
              [ txt "Here are some of our registered Github organisations" ];
          ];
      ]
  in
  Template_v1.instance
    [ title; div ~a:[ a_class [ "mt-8 flex flex-col space-y-6" ] ] org_table ]

let list_repos ~org ~repos =
  let github_org_url = github_org_url org in
  let repo_table =
    let repo_table_head =
      Common.table_head (Printf.sprintf "Repositories (%d)" (List.length repos))
    in
    let f { Client.Org.name; main_status; main_hash; main_last_updated } =
      let last_updated = Timestamps_durations.pp_timestamp main_last_updated in
      Dream.log "%s %s %s" name main_hash last_updated;
      Build.repo_row ~repo_title:name ~short_hash:(short_hash main_hash)
        ~last_updated ~status:main_status ~repo_uri:(repo_url org name)
    in
    repo_table_head :: List.map f repos
  in
  let title =
    div
      ~a:[ a_class [ "justify-between items-center flex" ] ]
      [
        div
          ~a:[ a_class [ "flex space-x-4" ] ]
          [
            img
              ~a:[ a_style "border-radius: 50%; width: 88px" ]
              ~src:(Printf.sprintf "https://github.com/%s.png?size=200" org)
              ~alt:(Printf.sprintf "%s profile picture" org)
              ();
            div
              ~a:[ a_class [ "flex flex-col" ] ]
              [
                h1 ~a:[ a_class [ "text-xl" ] ] [ txt org ];
                a
                  ~a:
                    [
                      a_class [ "text-sm flex items-center space-x-2" ];
                      a_href github_org_url;
                    ]
                  [ span [ txt github_org_url ]; Common.link_svg ];
              ];
          ];
      ]
  in
  Template_v1.instance
    [
      Common.breadcrumbs [ (prefix, prefix) ] org;
      title;
      Build.tabulate repo_table;
    ]

let list_refs ~org ~repo ~refs =
  let f
      {
        Client.Repo.name;
        hash;
        status;
        started = last_updated;
        message = _message;
      } =
    let short_hash = short_hash hash in
    let last_updated = Timestamps_durations.pp_timestamp last_updated in
    Build.ref_row ~ref_title:(ref_name name) ~short_hash ~last_updated ~status
      ~ref_uri:(commit_url ~org ~repo short_hash)
      ~message:short_hash
  in
  let default_table, main_ref =
    let main_ref, main_ref_info =
      Client.Ref_map.bindings refs
      |> List.find (fun (_, { Client.Repo.name; _ }) ->
             String.equal name "refs/heads/main"
             || String.equal name "refs/heads/master")
    in
    let table_head = Common.table_head "Default Branch" in
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
      Common.table_head (Printf.sprintf "Branches (%d)" n_branches)
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
      Common.table_head (Printf.sprintf "Refs Branches (%d)" n_prs)
    in
    let bindings = Client.Ref_map.bindings prs in
    let table = table_head :: List.map (fun (_, ref) -> f ref) bindings in
    (table, n_prs)
  in
  Dream.log "n_branches: %d - n_prs: %d" n_branches n_prs;
  let title =
    let github_repo_url = github_repo_url ~org repo in
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
                          a_href github_repo_url;
                        ]
                      [ span [ txt github_repo_url ]; Common.link_svg ];
                  ];
              ];
          ];
      ]
  in
  [
    Common.breadcrumbs [ (prefix, prefix); (org, org) ] repo;
    title;
    Build.tabulate default_table;
  ]
  |> (fun content ->
       if n_branches = 0 then content
       else content @ [ Build.tabulate branch_table ])
  |> (fun content ->
       if n_prs = 0 then content else content @ [ Build.tabulate pr_table ])
  |> Template_v1.instance

let list_history ~org ~repo ~ref ~history =
  let head_hash =
    match history with [] -> "" | (hash, _, _, _) :: _ -> hash
  in
  let commit_table =
    let commit_table_head =
      Common.table_head (Printf.sprintf "Builds (%d)" (List.length history))
    in
    let f (u, message, status, t) =
      let created_at = Timestamps_durations.pp_timestamp (Some t) in
      Build.commit_row ~commit_title:message ~short_hash:(short_hash u)
        ~created_at ~status ~commit_uri:(commit_url ~org ~repo u)
    in
    commit_table_head :: List.map f history
  in
  let title =
    div
      ~a:[ a_class [ "justify-between items-center flex" ] ]
      [
        div
          ~a:[ a_class [ "flex flex-items-center space-x-4" ] ]
          [
            div
              ~a:[ a_class [ "flex flex-col space-y-1" ] ]
              [
                h1
                  ~a:[ a_class [ "text-xl" ] ]
                  [
                    txt
                      (Printf.sprintf "Build History for \"%s\"" (ref_name ref));
                  ];
                div
                  ~a:[ a_class [ "text-gray-500" ] ]
                  [
                    div
                      ~a:[ a_class [ "flex text-sm space-x-2 " ] ]
                      [
                        txt
                          (Printf.sprintf
                             "Here is your build history for %s on %s"
                             (ref_name ref) repo);
                      ];
                  ];
              ];
          ];
      ]
  in
  Template_v1.instance
    [
      Common.breadcrumbs
        [
          (prefix, prefix);
          (org, org);
          (repo, repo);
          ref_breadcrumb ref head_hash;
        ]
        "Build History";
      title;
      Build.tabulate commit_table;
    ]

let cancel_success_message success =
  let format_job_info ji =
    li [ span [ txt @@ Printf.sprintf "Cancelling job: %s" ji.Client.variant ] ]
  in
  match success with
  | [] -> div [ span [ txt @@ Printf.sprintf "No jobs were cancelled." ] ]
  | success -> ul (List.map format_job_info success)

let cancel_success_message_v1 success =
  let format_job_info ji =
    (`Success, Printf.sprintf "Cancelling job: %s" ji.Client.variant)
  in
  match success with
  | [] -> [ (`Success, Printf.sprintf "No jobs were cancelled.") ]
  | success -> List.map format_job_info success

let cancel_fail_message = function
  | n when n <= 0 -> div []
  | 1 ->
      div
        [
          span
            [ txt "1 job could not be cancelled. Check logs for more detail." ];
        ]
  | n ->
      div
        [
          span
            [
              txt
              @@ Printf.sprintf
                   "%d jobs could not be cancelled. Check logs for more detail."
                   n;
            ];
        ]

let cancel_fail_message_v1 : int -> ([> `Fail ] * uri) list_wrap = function
  | n when n <= 0 -> []
  | 1 ->
      [ (`Fail, "1 job could not be cancelled. Check logs for more detail.") ]
  | n ->
      [
        ( `Fail,
          Printf.sprintf
            "%d jobs could not be cancelled. Check logs for more detail." n );
      ]

let rebuild_success_message success =
  let format_job_info ji =
    li [ span [ txt @@ Printf.sprintf "Rebuilding job: %s" ji.Client.variant ] ]
  in
  match success with
  | [] -> div [ span [ txt "No jobs were rebuilt." ] ]
  | success -> ul (List.map format_job_info success)

let rebuild_success_message_v1 success =
  let format_job_info ji =
    (`Success, Printf.sprintf "Rebuilding job: %s" ji.Client.variant)
  in
  match success with
  | [] -> [ (`Success, "No jobs were rebuilt.") ]
  | success -> List.map format_job_info success

let rebuild_fail_message = function
  | n when n <= 0 -> div []
  | 1 ->
      div
        [
          span [ txt "1 job could not be rebuilt. Check logs for more detail." ];
        ]
  | n ->
      div
        [
          span
            [
              txt
              @@ Printf.sprintf
                   "%d jobs could not be rebuilt. Check logs for more detail." n;
            ];
        ]

let rebuild_fail_message_v1 = function
  | n when n <= 0 -> []
  | 1 -> [ (`Fail, "1 job could not be rebuilt. Check logs for more detail.") ]
  | n ->
      [
        ( `Fail,
          Printf.sprintf
            "%d jobs could not be rebuilt. Check logs for more detail." n );
      ]

let return_link ~org ~repo ~hash =
  let uri = commit_url ~org ~repo hash in
  a ~a:[ a_href uri ] [ txt @@ Printf.sprintf "Return to %s" (short_hash hash) ]

(* TODO: Clean up so that success and fail messages appear in flash messages and we do a redirect
   instead of providing a return link *)
let list_steps ~org ~repo ~refs ~hash ~jobs ~first_step_queued_at
    ~total_run_time ?(flash_messages = [])
    ?(build_status : Client.State.t = Passed) ~csrf_token () =
  let can_cancel =
    let check job_info =
      match job_info.Client.outcome with
      | Active | NotStarted -> true
      | Aborted | Failed _ | Passed | Undefined _ -> false
    in
    List.exists check jobs
  in
  let can_rebuild =
    let check job_info =
      match job_info.Client.outcome with
      | Active | NotStarted | Passed -> false
      | Aborted | Failed _ | Undefined _ -> true
    in
    List.exists check jobs
  in
  let buttons =
    if can_cancel then [ Common.form_cancel ~hash ~csrf_token ]
    else if can_rebuild then Common.rebuild_button ~hash ~csrf_token
    else []
  in
  let title_card =
    (* FIXME [benmandrew]: [card_title] should be the commit message, but that isn't sent through yet *)
    Build.title_card ~status:build_status ~card_title:(short_hash hash)
      ~hash_link:(link_github_commit ~org ~repo ~hash:(short_hash hash))
      ~ref_links:(link_github_refs ~org ~repo refs)
      ~first_created_at:(Timestamps_durations.pp_timestamp first_step_queued_at)
      ~ran_for:(Timestamps_durations.pp_duration (Some total_run_time))
      ~buttons
  in
  let steps_table title jobs =
    List.fold_left
      (fun l j ->
        let build_created_at = Option.value ~default:0. first_step_queued_at in
        let ts = Result.to_option @@ Run_time.timestamps_from_job_info j in
        let rt =
          Option.map (Run_time.run_times_from_timestamps ~build_created_at) ts
        in
        let created_at = Timestamps_durations.pp_timestamp j.queued_at in
        let queued_for =
          Timestamps_durations.pp_duration (Option.map Run_time.queued_for rt)
        in
        let ran_for =
          Timestamps_durations.pp_duration (Option.map Run_time.ran_for rt)
        in
        let step_uri = job_url ~org ~repo ~hash j.variant in
        List.append l
          [
            Build.step_row ~step_title:j.variant ~created_at ~queued_for
              ~ran_for ~status:j.outcome ~step_uri;
          ])
      [ Common.table_head title ]
      jobs
  in
  let is_analysis_step variant =
    Astring.String.is_prefix ~affix:"(" variant
    && Astring.String.is_suffix ~affix:")" variant
  in
  let analysis_jobs =
    List.filter (fun (j : Client.job_info) -> is_analysis_step j.variant) jobs
  in
  let build_jobs =
    List.filter
      (fun (j : Client.job_info) -> not (is_analysis_step j.variant))
      jobs
  in
  Template_v1.instance
    [
      Common.breadcrumbs
        [ ("github", "github"); (org, org); (repo, repo) ]
        (Printf.sprintf "%s" (short_hash hash));
      title_card;
      Common.flash_messages flash_messages;
      Build.tabulate (steps_table "Analysis" analysis_jobs);
      Build.tabulate (steps_table "Build" build_jobs);
    ]

let show_step ~org ~repo ~refs ~hash ~jobs ~variant ~job ~status ~csrf_token
    ~timestamps ~build_created_at ?(flash_messages = []) (data, next) =
  let header, footer =
    let can_rebuild = status.Current_rpc.Job.can_rebuild in
    let button =
      if can_rebuild then Some (Common.form_rebuild_step ~variant ~csrf_token)
      else None
    in
    let branch =
      if refs = [] then ""
      else
        match Astring.String.cuts ~sep:"/" (List.hd refs) with
        | "refs" :: "heads" :: branch -> Astring.String.concat ~sep:"/" branch
        | _ -> ""
    in
    (* FIXME: This will throw an exception and cause a noisy 500 - is that ok? *)
    let step_info = List.find (fun j -> j.Client.variant = variant) jobs in
    let build_status = step_info.outcome in
    let build_created_at = Option.value ~default:0. build_created_at in
    let run_time =
      Option.map
        (Run_time.run_times_from_timestamps ~build_created_at)
        timestamps
    in
    let title_card =
      Step.title_card ~status:build_status ~card_title:variant
        ~hash_link:(link_github_commit ~org ~repo ~hash:(short_hash hash))
        ~created_at:(Timestamps_durations.pp_timestamp step_info.queued_at)
        ~finished_at:(Timestamps_durations.pp_timestamp step_info.finished_at)
        ~queued_for:
          (Timestamps_durations.pp_duration
             (Option.map Run_time.queued_for run_time))
        ~ran_for:
          (Timestamps_durations.pp_duration
             (Option.map Run_time.ran_for run_time))
        ~button
    in
    let body =
      Template_v1.instance ~scripts:Step.log_highlight_js
        [
          Common.breadcrumbs
            [
              ("github", "github");
              (org, org);
              (repo, repo);
              ( Printf.sprintf "%s (%s)" (short_hash hash) branch,
                Printf.sprintf "commit/%s" hash );
            ]
            variant;
          title_card;
          Common.flash_messages flash_messages;
          div
            ~a:
              [
                a_class [ "container-fluid mt-8 flex flex-col" ];
                Tyxml_helpers.x_data
                  "{ url: window.location.href, logs: true, artefacts: false, \
                   codeCoverage: false, codeCopied: false, linkCopied: false, \
                   startingLine: null, endingLine: null, manualSelection: \
                   false}";
              ]
            [
              div
                ~a:
                  [
                    a_class [ "notification" ];
                    Tyxml_helpers.x_cloak;
                    Tyxml_helpers.x_show "linkCopied";
                    Tyxml_helpers.x_transition;
                  ]
                [
                  div
                    ~a:[ a_class [ "flex items-center space-x-2" ] ]
                    [
                      div
                        ~a:[ a_class [ "icon-status icon-status--success" ] ]
                        [
                          Tyxml.Svg.(
                            Tyxml.Html.svg
                              ~a:
                                [
                                  a_class [ "h-4 w-4" ];
                                  a_viewBox (0., 0., 20., 20.);
                                  a_fill (`Color ("#12B76A", None));
                                ]
                              [
                                path
                                  ~a:
                                    [
                                      Tyxml_helpers.a_svg_custom "fill-rule"
                                        "evenodd";
                                      Tyxml_helpers.a_svg_custom "clip-rule"
                                        "evenodd";
                                      a_d
                                        "M16.707 5.293a1 1 0 010 1.414l-8 8a1 \
                                         1 0 01-1.414 0l-4-4a1 1 0 \
                                         011.414-1.414L8 12.586l7.293-7.293a1 \
                                         1 0 011.414 0z";
                                    ]
                                  [];
                              ]);
                        ];
                      div [ txt "Link copied" ];
                    ];
                  Tyxml.Html.button
                    ~a:
                      [
                        a_class [ "icon-button" ];
                        Tyxml_helpers.at_click "linkCopied=false";
                      ]
                    [
                      Tyxml.Svg.(
                        Tyxml.Html.svg
                          ~a:
                            [
                              a_fill `None;
                              a_viewBox (0., 0., 24., 24.);
                              a_stroke_width (2.5, Some `Px);
                              a_stroke `CurrentColor;
                              a_class [ "w-4 h-4" ];
                            ]
                          [
                            path
                              ~a:
                                [
                                  a_stroke_linecap `Round;
                                  a_stroke_linejoin `Round;
                                  a_d "M4.5 19.5l15-15m-15 0l15 15";
                                ]
                              [];
                          ]);
                    ];
                ];
              div
                ~a:
                  [
                    a_class
                      [ "flex space-x-6 border-b border-gray-200 mb-6 text-sm" ];
                  ]
                [ h3 ~a:[ a_class [ "font-medium pb-2" ] ] [ txt "Logs" ] ];
              div
                [
                  div
                    ~a:
                      [
                        a_class [ "mt-6 bg-gray-900 rounded-lg relative" ];
                        Tyxml_helpers.x_data "codeLink";
                        Tyxml_helpers.x_init "highlightLine";
                      ]
                    [
                      Tyxml.Html.button
                        ~a:
                          [
                            a_class [ "copy-link-btn" ];
                            Tyxml_helpers.at_click "copyCode";
                            Tyxml_helpers.x_show "manualSelection";
                            Tyxml_helpers.x_ref "copyLinkBtn";
                          ]
                        [
                          Tyxml.Svg.(
                            Tyxml.Html.svg
                              ~a:
                                [
                                  a_class [ "w-4 h-4" ];
                                  a_fill `None;
                                  a_viewBox (0., 0., 24., 24.);
                                  a_stroke_width (2., Some `Px);
                                  a_stroke `CurrentColor;
                                ]
                              [
                                path
                                  ~a:
                                    [
                                      a_stroke_linecap `Round;
                                      a_stroke_linejoin `Round;
                                      a_d
                                        "M13.19 8.688a4.5 4.5 0 011.242 \
                                         7.244l-4.5 4.5a4.5 4.5 0 \
                                         01-6.364-6.364l1.757-1.757m13.35-.622l1.757-1.757a4.5 \
                                         4.5 0 00-6.364-6.364l-4.5 4.5a4.5 4.5 \
                                         0 001.242 7.244";
                                    ]
                                  [];
                              ]);
                        ];
                      div
                        ~a:
                          [
                            a_class
                              [ "table-overflow overflow-auto rounded-lg" ];
                          ]
                        [ txt "@@@" ];
                    ];
                ];
            ];
        ]
    in

    Astring.String.cut ~sep:"@@@" body |> Option.get
  in
  let ansi = Ansi.create () in
  let line_number = ref 0 in
  let last_line_blank = ref false in
  let tabulate data : string =
    let aux (l : string) log_line =
      if !last_line_blank && log_line = "" then
        (* Squash consecutive new lines *)
        l
      else (
        last_line_blank := log_line = "";
        line_number := !line_number + 1;
        Fmt.str "%s\n%s" l
          (Fmt.str "%a" (pp_elt ())
             (tr
                ~a:
                  [
                    a_class [ "code-line" ];
                    Tyxml_helpers.colon_class
                      "parseInt($el.id.substring(1, $el.id.length)) >= \
                       startingLine && parseInt($el.id.substring(1, \
                       $el.id.length)) <= endingLine ? 'highlight' : ''";
                    Tyxml_helpers.at_click "highlightLine";
                    a_id (Fmt.str "L%d" !line_number);
                  ]
                [
                  td
                    ~a:[ a_class [ "code-line__number" ] ]
                    [ txt (Fmt.str "%d" !line_number) ];
                  td
                    ~a:[ a_class [ "code-line__code" ] ]
                    [ pre [ Unsafe.data log_line ] ];
                ])))
    in
    Fmt.str "%s%s" (List.fold_left aux "<table><tbody>" data) "</tbody></table>"
  in
  let open Lwt.Infix in
  Dream.stream
    ~headers:[ ("Content-type", "text/html; charset=utf-8") ]
    (fun response_stream ->
      Dream.write response_stream header >>= fun () ->
      let data' =
        data |> Ansi.process ansi |> Astring.String.cuts ~sep:"\n" |> tabulate
      in
      Dream.write response_stream data' >>= fun () ->
      let rec loop next =
        Current_rpc.Job.log job ~start:next >>= function
        | Ok ("", _) ->
            Dream.write response_stream footer >>= fun () ->
            Dream.close response_stream
        | Ok (data, next) ->
            Dream.log "Fetching logs";
            let data' =
              data
              |> Ansi.process ansi
              |> Astring.String.cuts ~sep:"\n"
              |> tabulate
            in
            Dream.write response_stream data' >>= fun () ->
            Dream.flush response_stream >>= fun () -> loop next
        | Error (`Capnp ex) ->
            Dream.log "Error fetching logs: %a" Capnp_rpc.Error.pp ex;
            Dream.write response_stream
              (Fmt.str "ocaml-ci error: %a@." Capnp_rpc.Error.pp ex)
      in
      loop next)
