module Client = Ocaml_ci_api.Client
module StatusTree = Status_tree
module Build_status = Build_status
module Run_time = Ocaml_ci_client_lib.Run_time
open Tyxml.Html
open Git_forge

let prefix = "github"

module Ref = Ref.Make (struct
  let prefix = prefix
end)

module Repo = Repo.Make (struct
  let prefix = prefix
end)

let github_branch_url ~org ~repo ref =
  Printf.sprintf "https://github.com/%s/%s/tree/%s" org repo ref

let github_commit_url ~org ~repo ~hash =
  Printf.sprintf "https://github.com/%s/%s/commit/%s" org repo hash

let github_pr_url ~org ~repo id =
  Printf.sprintf "https://github.com/%s/%s/pull/%s" org repo id

(* let github_repo_url ~org repo =
   Printf.sprintf "https://github.com/%s/%s" org repo *)

let format_org org =
  li [ a ~a:[ a_href (Url.org_url prefix ~org) ] [ txt org ] ]

let orgs_v ~orgs = [ breadcrumbs [] prefix; ul (List.map format_org orgs) ]

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

let link_github_commit ~org ~repo ~hash =
  a ~a:[ a_href (github_commit_url ~org ~repo ~hash) ] [ txt (short_hash hash) ]

let link_github_refs ~org ~repo = function
  | [] -> txt "(not at the head of any monitored branch or PR)"
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
                           ~a:[ a_href (github_branch_url ~org ~repo branch) ]
                           [ txt branch ];
                       ]
                 | [ "refs"; "pull"; id; "head" ] ->
                     span
                       [
                         txt "PR ";
                         a
                           ~a:[ a_href (github_pr_url ~org ~repo id) ]
                           [ txt ("#" ^ id) ];
                       ]
                 | _ -> txt (Fmt.str "Bad ref format %S" r))
        @ [ txt ")" ])

let link_github_refs' ~org ~repo refs =
  let f r =
    match Astring.String.cuts ~sep:"/" r with
    | "refs" :: "heads" :: branch ->
        let branch = Astring.String.concat ~sep:"/" branch in
        a
          ~a:
            [
              a_class [ "flex items-center space-x-2" ];
              a_href (github_branch_url ~org ~repo branch);
            ]
          [ span [ txt branch ]; Common.external_link ]
    | [ "refs"; "pull"; id; "head" ] ->
        a
          ~a:
            [
              a_class [ "flex items-center space-x-2" ];
              a_href (github_pr_url ~org ~repo id);
            ]
          [ span [ txt ("PR#" ^ id) ]; Common.external_link ]
    | _ -> txt ""
  in
  List.map f refs

let list_orgs ~orgs = Template.instance @@ orgs_v ~orgs
let list_repos ~org ~repos = Repo.list ~org ~repos
let list_refs = Ref.list

let list_history ~org ~repo ~ref ~history =
  Template.instance
    [
      breadcrumbs [ ("github", "github"); (org, org) ] repo;
      link_github_refs ~org ~repo [ ref ];
      history_v ~org ~repo ~history;
    ]

let cancel_success_message success =
  let format_job_info ji =
    li [ span [ txt @@ Printf.sprintf "Cancelling job: %s" ji.Client.variant ] ]
  in
  match success with
  | [] -> div [ span [ txt "No jobs were cancelled." ] ]
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
        Astring.String.concat ~sep:", "
          (List.map (fun ji -> ji.Client.variant) trimmed)
      in
      if List.length jis > 5 then
        [
          ( `Success,
            Astring.String.concat [ prefix; " many: "; message; " ..." ] );
        ]
      else [ (`Success, Astring.String.concat [ prefix; ": "; message ]) ]

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

let list_steps ~org ~repo ~message ~refs ~hash ~jobs ~first_step_queued_at
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
  let show_rebuild = (not can_cancel) && can_rebuild in
  let buttons =
    Common.form_cancel ~hash ~csrf_token ~show:can_cancel ()
    :: Common.rebuild_button ~hash ~csrf_token ~show:show_rebuild ()
  in
  let title_card =
    Build.title_card ~status:build_status ~card_title:message
      ~hash_link:(link_github_commit ~org ~repo ~hash)
      ~ref_links:(link_github_refs' ~org ~repo refs)
      ~first_created_at:(Timestamps_durations.pp_timestamp first_step_queued_at)
      ~ran_for:(Timestamps_durations.pp_duration (Some total_run_time))
      ~buttons
  in
  let steps_table_div =
    div
      ~a:
        [ a_class [ "bg-gray-50 px-6 py-3 text-gray-500 text-xs font-medium" ] ]
        (* TODO: We need to start with no stage separation - introduce Analysis/Checks and Build steps later *)
      [ txt "Build" ]
  in
  let steps_table =
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
        let step_uri = Url.job_url prefix ~org ~repo ~hash j.variant in
        List.append l
          [
            Build.step_row ~step_title:j.variant ~created_at ~queued_for
              ~ran_for ~status:j.outcome ~step_uri;
          ])
      [ steps_table_div ] jobs
  in
  Template_v1.instance
    [
      Tyxml.Html.script ~a:[ a_src "/js/build-page-poll.js" ] (txt "");
      Common.breadcrumbs
        [ ("github", "github"); (org, org); (repo, repo) ]
        (Printf.sprintf "%s" (short_hash hash));
      title_card;
      Common.flash_messages flash_messages;
      Build.tabulate_steps steps_table;
    ]

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
        ~hash_link:(link_github_commit ~org ~repo ~hash)
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
