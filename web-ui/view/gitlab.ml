module Client = Ocaml_ci_api.Client
module Common_api = Ocaml_ci_api.Common
module StatusTree = Status_tree
module Build_status = Build_status
module Run_time = Ocaml_ci_client_lib.Run_time
open Tyxml.Html
open Git_forge

let prefix = "gitlab"

module Ref = Ref.Make (struct
  let prefix = prefix
end)

let gitlab_branch_url ~org ~repo ref =
  Fmt.str "https://gitlab.com/%s/%s/-/tree/%s" org repo ref

let gitlab_mr_url ~org ~repo id =
  Fmt.str "https://gitlab.com/%s/%s/-/merge_requests/%s" org repo id

let gitlab_commit_url ~org ~repo ~hash =
  Printf.sprintf "https://gitlab.com/%s/%s/-/commit/%s" org repo hash

let format_org org =
  li [ a ~a:[ a_href (Url.org_url prefix ~org) ] [ txt org ] ]

let format_repo ~org { Client.Org.name; master_status } =
  li
    ~a:[ a_class [ Build_status.class_name master_status ] ]
    [ a ~a:[ a_href (Url.repo_url prefix ~org ~repo:name) ] [ txt name ] ]

let orgs_v ~orgs = [ breadcrumbs [] prefix; ul (List.map format_org orgs) ]

let repos_v ~org ~repos =
  [
    breadcrumbs [ (prefix, prefix) ] org;
    ul ~a:[ a_class [ "statuses" ] ] (List.map (format_repo ~org) repos);
  ]

let refs_v ~org ~repo ~refs =
  ul
    ~a:[ a_class [ "statuses" ] ]
    (Client.Ref_map.bindings refs
    |> List.map @@ fun (branch, { Client.Repo.hash; status; _ }) ->
       li
         ~a:[ a_class [ Build_status.class_name status ] ]
         [
           a
             ~a:[ a_href (Url.commit_url prefix ~org ~repo ~hash) ]
             [ txt branch ];
         ])

let history_v ~org ~repo ~history =
  ul
    ~a:[ a_class [ "statuses" ] ]
    (history
    |> List.map @@ fun (commit, status) ->
       li
         ~a:[ a_class [ Build_status.class_name status ] ]
         [
           a
             ~a:[ a_href (Url.commit_url prefix ~org ~repo ~hash:commit) ]
             [ txt commit ];
         ])

let link_gitlab_commit ~org ~repo ~hash =
  a ~a:[ a_href (gitlab_commit_url ~org ~repo ~hash) ] [ txt hash ]

let link_gitlab_refs ~org ~repo = function
  | [] -> txt "(not at the head of any monitored branch or merge request)"
  | refs ->
      p
        (txt "(for "
         :: intersperse ~sep:(txt ", ")
              (refs
              |> List.map @@ fun r ->
                 match Astring.String.cuts ~sep:"/" r with
                 | "refs" :: "heads" :: branch ->
                     let branch = Astring.String.concat ~sep:"/" branch in
                     span
                       [
                         txt "branch ";
                         a
                           ~a:[ a_href (gitlab_branch_url ~org ~repo branch) ]
                           [ txt branch ];
                       ]
                 | [ "refs"; "merge-requests"; id; "head" ] ->
                     span
                       [
                         txt "Merge Request ";
                         a
                           ~a:[ a_href (gitlab_mr_url ~org ~repo id) ]
                           [ txt ("#" ^ id) ];
                       ]
                 | _ -> txt (Fmt.str "Bad ref format %S" r))
        @ [ txt ")" ])

let link_jobs ~org ~repo ~hash ?selected jobs =
  let render_job trees { Client.variant; outcome; _ } =
    let uri = Url.job_url prefix ~org ~repo ~hash variant in
    match
      List.rev
        (Astring.String.cuts ~sep:(Fmt.str "%c" Common_api.status_sep) variant)
    with
    | [] -> assert false
    | label_txt :: k ->
        let k = List.rev k in
        let x =
          let label =
            txt (Fmt.str "%s (%a)" label_txt Client.State.pp outcome)
          in
          let label = if selected = Some variant then b [ label ] else label in
          (outcome, [ a ~a:[ a_href uri ] [ label ] ])
        in
        StatusTree.add k x trees
  in
  statuses (List.fold_left render_job [] jobs)

let list_orgs ~orgs = Template.instance @@ orgs_v ~orgs
let list_repos ~org ~repos = Template.instance @@ repos_v ~org ~repos

let list_refs ~org ~repo ~refs =
  Template.instance
    [
      breadcrumbs [ (prefix, prefix); (org, org) ] repo; refs_v ~org ~repo ~refs;
    ]

let list_history ~org ~repo ~ref ~history =
  Template.instance
    [
      breadcrumbs [ (prefix, prefix); (org, org) ] repo;
      link_gitlab_refs ~org ~repo [ ref ];
      history_v ~org ~repo ~history;
    ]

let cancel_success_message success =
  let format_job_info ji =
    li [ span [ txt @@ Fmt.str "Cancelling job: %s" ji.Client.variant ] ]
  in
  match success with
  | [] -> div [ span [ txt @@ Fmt.str "No jobs were cancelled." ] ]
  | success -> ul (List.map format_job_info success)

let cancel_fail_message = function
  | n when n <= 0 -> div []
  | 1 ->
      div
        [
          span
            [
              txt
              @@ Fmt.str
                   "1 job could not be cancelled. Check logs for more detail.";
            ];
        ]
  | n ->
      div
        [
          span
            [
              txt
              @@ Fmt.str
                   "%d jobs could not be cancelled. Check logs for more detail."
                   n;
            ];
        ]

let cancel_fail_message_v1 : int -> ([> `Fail ] * uri) list_wrap = function
  | n when n <= 0 -> []
  | 1 ->
      [
        ( `Fail,
          Fmt.str "1 job could not be cancelled. Check logs for more detail." );
      ]
  | n ->
      [
        ( `Fail,
          Fmt.str "%d jobs could not be cancelled. Check logs for more detail."
            n );
      ]

let rebuild_success_message success =
  let format_job_info ji =
    li [ span [ txt @@ Fmt.str "Rebuilding job: %s" ji.Client.variant ] ]
  in
  match success with
  | [] -> div [ span [ txt @@ Fmt.str "No jobs were rebuilt." ] ]
  | success -> ul (List.map format_job_info success)

let success_message_v1 action jis =
  let empty_message, prefix =
    match action with
    | `Cancel -> ("No jobs were cancelled.", "Cancelling")
    | `Rebuild -> ("No jobs were rebuilt.", "Rebuilding")
  in
  match jis with
  | [] -> [ (`Success, empty_message) ]
  | jis ->
      let trimmed = List.filteri (fun i _ -> i < 5) jis in
      let message =
        Astring.String.concat ~sep:"\x0C, "
          (List.map (fun ji -> ji.Client.variant) trimmed)
      in
      if List.length jis > 5 then
        [
          ( `Success,
            Astring.String.concat [ prefix; " many: "; message; " ..." ] );
        ]
      else [ (`Success, Astring.String.concat [ prefix; ": "; message ]) ]

let rebuild_fail_message = function
  | n when n <= 0 -> div []
  | 1 ->
      div
        [
          span
            [
              txt
              @@ Fmt.str
                   "1 job could not be rebuilt. Check logs for more detail.";
            ];
        ]
  | n ->
      div
        [
          span
            [
              txt
              @@ Fmt.str
                   "%d jobs could not be rebuilt. Check logs for more detail." n;
            ];
        ]

let rebuild_fail_message_v1 : int -> ([> `Fail ] * uri) list_wrap = function
  | n when n <= 0 -> []
  | 1 ->
      [
        ( `Fail,
          Fmt.str "1 job could not be rebuilt. Check logs for more detail." );
      ]
  | n ->
      [
        ( `Fail,
          Fmt.str "%d jobs could not be rebuilt. Check logs for more detail." n
        );
      ]

let list_steps ~org ~repo ~message ~refs ~hash ~jobs ~first_step_queued_at
    ~total_run_time ?(flash_messages = [])
    ?(build_status : Client.State.t = Passed) ~csrf_token () =
  ignore message;
  let () = ignore build_status in
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
    if can_cancel then
      [
        form
          ~a:[ a_action (hash ^ "/cancel"); a_method `Post ]
          [
            Unsafe.data csrf_token;
            input ~a:[ a_input_type `Submit; a_value "Cancel" ] ();
          ];
      ]
    else if can_rebuild then
      [
        form
          ~a:[ a_action (hash ^ "/rebuild-failed"); a_method `Post ]
          [
            Unsafe.data csrf_token;
            button [ txt "Rebuild Failed" ];
            input
              ~a:[ a_name "filter"; a_input_type `Hidden; a_value "failed" ]
              ();
          ];
        form
          ~a:[ a_action (hash ^ "/rebuild-all"); a_method `Post ]
          [
            Unsafe.data csrf_token;
            button [ txt "Rebuild All" ];
            input
              ~a:[ a_name "filter"; a_input_type `Hidden; a_value "none" ]
              ();
          ];
      ]
    else []
  in
  Template.instance ~flash_messages
    [
      breadcrumbs
        [ (prefix, prefix); (org, org); (repo, repo) ]
        (short_hash hash);
      link_gitlab_refs ~org ~repo refs;
      link_jobs ~org ~repo ~hash jobs;
      Timestamps_durations.show_build ~first_step_queued_at ~total_run_time;
      div buttons;
    ]

let _show_step ~org ~repo ~refs ~hash ~jobs ~variant ~job ~status ~csrf_token
    ~timestamps ~build_created_at ?(flash_messages = []) (data, next) =
  let header, footer =
    let can_rebuild = status.Current_rpc.Job.can_rebuild in
    let buttons =
      if can_rebuild then
        [
          form
            ~a:[ a_action (variant ^ "/rebuild"); a_method `Post ]
            [
              Unsafe.data csrf_token;
              input ~a:[ a_input_type `Submit; a_value "Rebuild" ] ();
            ];
        ]
      else []
    in
    let body =
      Template.instance ~flash_messages
        [
          breadcrumbs
            [
              (prefix, prefix);
              (org, org);
              (repo, repo);
              (short_hash hash, "commit/" ^ hash);
            ]
            variant;
          link_gitlab_refs ~org ~repo refs;
          link_jobs ~org ~repo ~hash ~selected:variant jobs;
          Timestamps_durations.show_step ~build_created_at timestamps;
          div buttons;
          pre [ txt "@@@" ];
        ]
    in
    Astring.String.cut ~sep:"@@@" body |> Option.get
  in
  let ansi = Ansi.create () in
  let open Lwt.Infix in
  Dream.stream
    ~headers:[ ("Content-type", "text/html; charset=utf-8") ]
    (fun response_stream ->
      Dream.write response_stream header >>= fun () ->
      Dream.write response_stream (Ansi.process ansi data) >>= fun () ->
      let rec loop next =
        Current_rpc.Job.log job ~start:next >>= function
        | Ok ("", _) ->
            Dream.write response_stream footer >>= fun () ->
            Dream.close response_stream
        | Ok (data, next) ->
            Dream.log "Fetching logs";
            Dream.write response_stream (Ansi.process ansi data) >>= fun () ->
            Dream.flush response_stream >>= fun () -> loop next
        | Error (`Capnp ex) ->
            Dream.log "Error fetching logs: %a" Capnp_rpc.Error.pp ex;
            Dream.write response_stream
              (Fmt.str "ocaml-ci error: %a@." Capnp_rpc.Error.pp ex)
      in
      loop next)

let show_step ~org ~repo ~refs ~hash ~jobs ~variant ~job ~status ~csrf_token
    ~timestamps ~build_created_at ?(flash_messages = []) (data, next) =
  let header, footer =
    let can_rebuild = status.Current_rpc.Job.can_rebuild in
    let button =
      Some (Common.form_rebuild_step ~variant ~csrf_token ~show:can_rebuild ())
    in
    let branch =
      if refs = [] then ""
      else
        match Astring.String.cuts ~sep:"/" (List.hd refs) with
        | "refs" :: "heads" :: branch -> Astring.String.concat ~sep:"/" branch
        | _ -> ""
    in
    (* FIXME: This will throw an exception and cause a noisy 500 *)
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
        ~hash_link:(link_gitlab_commit ~org ~repo ~hash:(short_hash hash))
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
      Template_v1.instance
        [
          Tyxml.Html.script ~a:[ a_src "/js/log-highlight.js" ] (txt "");
          Tyxml.Html.script ~a:[ a_src "/js/step-page-poll.js" ] (txt "");
          Common.breadcrumbs
            [
              ("gitlab", "gitlab");
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
                            Tyxml_helpers.x_cloak;
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
        Printf.sprintf "%s\n%s" l
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
                    a_id (Printf.sprintf "L%d" !line_number);
                  ]
                [
                  td
                    ~a:[ a_class [ "code-line__number" ] ]
                    [ txt (Printf.sprintf "%d" !line_number) ];
                  td
                    ~a:[ a_class [ "code-line__code" ] ]
                    [ pre [ Unsafe.data log_line ] ];
                ])))
    in
    Printf.sprintf "%s%s"
      (List.fold_left aux "<table><tbody>" data)
      "</tbody></table>"
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
