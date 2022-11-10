module Client = Ocaml_ci_api.Client
module Common = Ocaml_ci_api.Common
module StatusTree = Status_tree
module Build_status = Build_status
module Run_time = Ocaml_ci_client_lib.Run_time
open Tyxml.Html
open Git_forge

let prefix = "gitlab"

module Ref = Ref.Make (struct
  let prefix = prefix
end)

let org_url org = Fmt.str "/%s/%s" prefix org
let repo_url org repo = Fmt.str "/%s/%s/%s" prefix org repo

let commit_url ~org ~repo hash =
  Fmt.str "/%s/%s/%s/commit/%s" prefix org repo hash

let job_url ~org ~repo ~hash variant =
  Fmt.str "/%s/%s/%s/commit/%s/variant/%s" prefix org repo hash variant

let gitlab_branch_url ~org ~repo ref =
  Fmt.str "https://gitlab.com/%s/%s/-/tree/%s" org repo ref

let gitlab_mr_url ~org ~repo id =
  Fmt.str "https://gitlab.com/%s/%s/-/merge_requests/%s" org repo id

let format_org org = li [ a ~a:[ a_href (org_url org) ] [ txt org ] ]

let format_repo ~org { Client.Org.name; master_status } =
  li
    ~a:[ a_class [ Build_status.class_name master_status ] ]
    [ a ~a:[ a_href (repo_url org name) ] [ txt name ] ]

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
         [ a ~a:[ a_href (commit_url ~org ~repo hash) ] [ txt branch ] ])

let history_v ~org ~repo ~history =
  ul
    ~a:[ a_class [ "statuses" ] ]
    (history
    |> List.map @@ fun (commit, status) ->
       li
         ~a:[ a_class [ Build_status.class_name status ] ]
         [ a ~a:[ a_href (commit_url ~org ~repo commit) ] [ txt commit ] ])

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
    let uri = job_url ~org ~repo ~hash variant in
    match
      List.rev
        (Astring.String.cuts ~sep:(Fmt.str "%c" Common.status_sep) variant)
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

let return_link ~org ~repo ~hash =
  let uri = commit_url ~org ~repo hash in
  a ~a:[ a_href uri ] [ txt @@ Fmt.str "Return to %s" (short_hash hash) ]

(* TODO: Clean up so that success and fail messages appear in flash messages and we do a redirect
   instead of providing a return link *)
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

let show_step ~org ~repo ~refs ~hash ~jobs ~variant ~job ~status ~csrf_token
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
