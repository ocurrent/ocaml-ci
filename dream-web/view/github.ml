module Client = Ocaml_ci_api.Client
module Common = Ocaml_ci_api.Common
module Build_status = struct
  include Client.Build_status

  let class_name (t : t) =
    match t with
    | NotStarted -> "not-started"
    | Failed -> "failed"
    | Passed -> "passed"
    | Pending -> "active"
    | Undefined _ -> "undefined"
end

module StatusTree : sig
  type key = string

  type 'a tree = Leaf of 'a | Branch of key * 'a t
  and 'a t = 'a tree list

  val add : key list -> 'a -> 'a t -> 'a t
end = struct
  type key = string

  type 'a tree = Leaf of 'a | Branch of key * 'a t
  and 'a t = 'a tree list

  let rec add k x ts =
    match (k, ts) with
    | [], ts -> ts @ [ Leaf x ]
    | k :: ks, [] -> [ Branch (k, add ks x []) ]
    | _ :: _, (Leaf _ as t) :: ts -> t :: add k x ts
    | k :: ks, Branch (k', t) :: ts when Astring.String.equal k k' ->
        Branch (k, add ks x t) :: ts
    | _ :: _, (Branch _ as t) :: ts -> t :: add k x ts
end

let short_hash = Astring.String.with_range ~len:6

let breadcrumbs steps page_title =
  let open Tyxml.Html in
  let add (prefix, results) (label, link) =
    let prefix = Fmt.str "%s/%s" prefix link in
    let link = li [a ~a:[a_href prefix] [txt label]] in
    (prefix, link :: results)
  in
  let _, steps = List.fold_left add ("", []) steps in
  let steps = li [b [txt page_title]] :: steps in
  ol ~a:[a_class ["breadcrumbs"]] (
    List.rev steps
  )

let org_url org =
  Fmt.str "/github/%s" org

let repo_url org repo =
  Fmt.str "/github/%s/%s" org repo

let commit_url ~org ~repo hash =
  Fmt.str "/github/%s/%s/commit/%s" org repo hash

let job_url ~org ~repo ~hash variant =
  Fmt.str "/github/%s/%s/commit/%s/variant/%s" org repo hash variant

let github_branch_url ~org ~repo ref =
  Fmt.str "https://github.com/%s/%s/tree/%s" org repo ref

let github_pr_url ~org ~repo id =
  Fmt.str "https://github.com/%s/%s/pull/%s" org repo id

let format_org org =
  let open Tyxml.Html in
  li [a ~a:[a_href (org_url org)] [txt org]]

let format_repo ~org { Client.Org.name; master_status } =
  let open Tyxml.Html in
  li ~a:[a_class [Build_status.class_name master_status]] [
    a ~a:[a_href (repo_url org name)] [txt name]
  ]

let orgs_v ~orgs =
    Tyxml.Html.[
      breadcrumbs [] "github";
      ul (List.map format_org orgs)
    ]

let repos_v ~org ~repos =
    Tyxml.Html.[
      breadcrumbs ["github", "github"] org;
      ul ~a:[a_class ["statuses"]] (List.map (format_repo ~org) repos)
    ]

let refs_v ~org ~repo ~refs =
  let open Tyxml.Html in
  ul ~a:[a_class ["statuses"]] (
    Client.Ref_map.bindings refs |> List.map @@ fun (branch, (commit, status)) ->
    li ~a:[a_class [Build_status.class_name status]] [
      a ~a:[a_href (commit_url ~org ~repo commit)] [txt branch]
    ]
  )

let rec intersperse ~sep = function
  | [] -> []
  | [ x ] -> [ x ]
  | x :: xs -> x :: sep :: intersperse ~sep xs

let statuses ss =
  let open Tyxml.Html in
  let rec render_status = function
    | StatusTree.Leaf (s, elms) ->
        let status_class_name =
          match (s : Client.State.t) with
          | NotStarted -> "not-started"
          | Aborted -> "aborted"
          | Failed m when Astring.String.is_prefix ~affix:"[SKIP]" m ->
              "skipped"
          | Failed _ -> "failed"
          | Passed -> "passed"
          | Active -> "active"
          | Undefined _ -> "undefined"
        in
        li ~a:[ a_class [ status_class_name ] ] elms
    | StatusTree.Branch (b, ss) ->
        li
          [
            txt b; ul ~a:[ a_class [ "statuses" ] ] (List.map render_status ss);
          ]
  in
  ul ~a:[ a_class [ "statuses" ] ] (List.map render_status ss)

let link_github_refs ~org ~repo =
  let open Tyxml.Html in
  function
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

let link_jobs ~org ~repo ~hash ?selected jobs =
  let open Tyxml.Html in
  let render_job trees { Client.variant; outcome } =
    let uri = job_url ~org ~repo ~hash variant in
    match List.rev (Astring.String.cuts ~sep:(Fmt.str "%c" Common.status_sep) variant) with
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
  let list_orgs ~orgs =
  Template_tyxml.instance @@ orgs_v ~orgs

let list_repos ~org ~repos =
  Template_tyxml.instance @@ repos_v ~org ~repos

let list_refs ~org ~repo ~refs =
  Template_tyxml.instance [
      breadcrumbs ["github", "github";
                   org, org] repo;
      refs_v ~org ~repo ~refs
    ]

let cancel_success_message success =
  let open Tyxml.Html in
  let format_job_info ji =
    li [span [txt @@ Fmt.str "Cancelling job: %s" ji.Client.variant]]
  in
  match success with
  | [] -> div [span [txt @@ Fmt.str "No jobs were cancelled."]]
  | success -> ul (List.map format_job_info success)

let cancel_fail_message failed =
  let open Tyxml.Html in
  match failed with
  | n when n <= 0 -> div []
  | 1 ->  div [span [txt @@ Fmt.str "1 job could not be cancelled. Check logs for more detail."]]
  | n ->  div [span [txt @@ Fmt.str "%d jobs could not be cancelled. Check logs for more detail." n]]

let rebuild_success_message success =
  let open Tyxml.Html in
  let format_job_info ji =
    li [span [txt @@ Fmt.str "Rebuilding job: %s" ji.Client.variant]]
  in
  match success with
  | [] -> div [span [txt @@ Fmt.str "No jobs were rebuilt."]]
  | success -> ul (List.map format_job_info success)

let rebuild_fail_message failed =
  let open Tyxml.Html in
  match failed with
  | n when n <= 0 -> div []
  | 1 ->  div [span [txt @@ Fmt.str "1 job could not be rebuilt. Check logs for more detail."]]
  | n ->  div [span [txt @@ Fmt.str "%d jobs could not be rebuilt. Check logs for more detail." n]]

let return_link ~org ~repo ~hash =
  let open Tyxml.Html in
  let uri = commit_url ~org ~repo hash in
  a ~a:[a_href uri] [txt @@ Fmt.str "Return to %s" (short_hash hash)]

let list_steps
  ~org ~repo ~refs ~hash ~jobs
  ?(success_msg=(let open Tyxml.Html in div[]))
  ?(fail_msg=(let open Tyxml.Html in div[]))
  ?(return_link=(let open Tyxml.Html in div[]))
  ?(flash_messages=[])
  ~csrf_token () =
  let open Tyxml.Html in
  let can_cancel = List.fold_left (fun accum job_info ->
    accum ||
      match job_info.Client.outcome with
      | Active | NotStarted -> true
      | Aborted | Failed _ | Passed | Undefined _ -> false) false jobs
  in
  let can_rebuild = List.fold_left (fun accum job_info ->
    accum ||
    match job_info.Client.outcome with
    | Active | NotStarted | Passed -> false
    | Aborted | Failed _ | Undefined _ -> true) false jobs
  in
  let buttons =
    if can_cancel then [
        form ~a:[a_action (hash ^ "/cancel"); a_method `Post] [
          input ~a:[a_name "csrf"; a_input_type `Hidden; a_value csrf_token] ();
          input ~a:[a_input_type `Submit; a_value "Cancel"] () ]
    ] else if can_rebuild then [
      form ~a:[a_action (hash ^ "/rebuild-failed"); a_method `Post] [
        button [txt "Rebuild Failed"];
        input ~a:[a_name "csrf"; a_input_type `Hidden; a_value csrf_token] ();
        input ~a:[a_name "filter"; a_input_type `Hidden; a_value "failed"] ()
      ];
      form ~a:[a_action (hash ^ "/rebuild-all"); a_method `Post] [
        button [txt "Rebuild All"];
        input ~a:[a_name "csrf"; a_input_type `Hidden; a_value csrf_token] ();
        input ~a:[a_name "filter"; a_input_type `Hidden; a_value "none"] ()
      ];
    ] else []
  in
  Template_tyxml.instance ~flash_messages [
        breadcrumbs ["github", "github";
                     org, org;
                     repo, repo] (short_hash hash);
        link_github_refs ~org ~repo refs;
        link_jobs ~org ~repo ~hash jobs;
        success_msg;
        fail_msg;
        return_link;
        div buttons;
      ]

let show_step ~org ~repo ~refs ~hash ~jobs ~variant ~job ~status ~csrf_token ?(flash_messages=[]) (data, next) =
  let header, footer =
    let can_rebuild = status.Current_rpc.Job.can_rebuild in
    let buttons =
      if can_rebuild then Tyxml.Html.[
          form ~a:[a_action (variant ^ "/rebuild"); a_method `Post] [
            input ~a:[a_name "csrf"; a_input_type `Hidden; a_value csrf_token] ();
            input ~a:[a_input_type `Submit; a_value "Rebuild"] ()
          ]
      ] else []
    in
    let body = Template_tyxml.instance ~flash_messages Tyxml.Html.[
        breadcrumbs ["github", "github";
                     org, org;
                     repo, repo;
                     short_hash hash, "commit/" ^ hash;
                    ] variant;
        link_github_refs ~org ~repo refs;
        link_jobs ~org ~repo ~hash ~selected:variant jobs;
        div buttons;
        pre [txt "@@@"]
  ] in
  Astring.String.cut ~sep:"@@@" body |> Option.get in
  let ansi = Ansi.create () in
  let open Lwt.Infix in
  Dream.stream
    ~headers:[ ("Content-type", "text/html; charset=utf-8") ]
    (fun response_stream ->
      let%lwt () = Dream.write response_stream header in
      let%lwt () = Dream.write response_stream (Ansi.process ansi data) in
      let rec loop next =
        Current_rpc.Job.log job ~start:next >>= function
        | Ok("", _) ->
          let%lwt () = Dream.write response_stream footer in
          Dream.close response_stream
        | Ok(data, next) -> (
            Dream.log "Fetching logs";
            let%lwt () = Dream.write response_stream (Ansi.process ansi data) in
            let%lwt () = Dream.flush response_stream in
            loop next)
        | Error (`Capnp ex) ->
            Dream.log "Error fetching logs: %a" Capnp_rpc.Error.pp ex;
            Dream.write response_stream
              (Fmt.str "ocaml-ci error: %a@." Capnp_rpc.Error.pp ex)
      in
      loop next)
