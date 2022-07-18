module Client = Ocaml_ci_api.Client
module Common = Ocaml_ci_api.Common
open Tyxml.Html
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
    | k :: ks, Branch (k', t) :: ts when String.equal k k' ->
        Branch (k, add ks x t) :: ts
    | _ :: _, (Branch _ as t) :: ts -> t :: add k x ts
end

let short_hash = Astring.String.with_range ~len:6

let breadcrumbs steps page_title =
  let add (prefix, results) (label, link) =
    let prefix = Printf.sprintf "%s/%s" prefix link in
    let link = li [a ~a:[a_href prefix] [txt label]] in
    (prefix, link :: results)
  in
  let _, steps = List.fold_left add ("", []) steps in
  let steps = li [b [txt page_title]] :: steps in
  ol ~a:[a_class ["breadcrumbs"]] (
    List.rev steps
  )

let org_url org =
  Printf.sprintf "/github/%s" org

let repo_url org repo =
  Printf.sprintf "/github/%s/%s" org repo

let format_org org =
  let url = org_url org in
  <li>
    <a href=<%s url %>><%s org %></a>
  </li>

let commit_url ~org ~repo hash =
  Printf.sprintf "/github/%s/%s/commit/%s" org repo hash

let job_url ~org ~repo ~hash variant =
  Printf.sprintf "/github/%s/%s/commit/%s/variant/%s" org repo hash variant

let github_branch_url ~org ~repo ref =
  Printf.sprintf "https://github.com/%s/%s/tree/%s" org repo ref

let github_pr_url ~org ~repo id =
  Printf.sprintf "https://github.com/%s/%s/pull/%s" org repo id


let format_repo ~org repo_info =
  let repo = repo_info.Client.Org.name in
  let url = repo_url org repo in
  let build_status = Build_status.class_name repo_info.master_status in
  <li class=<%s build_status %>>
    <a href=<%s url %>><%s repo %></a>
  </li>

let orgs_v ~orgs =
  let lis = String.concat "" (List.map format_org orgs) in
  <ul>
    <%s! lis %>
  </ul>

let repos_v ~org ~repos =
  let lis = String.concat "" (List.map (format_repo ~org) repos) in
  <ul class="statuses">
    <%s! lis %>
  </ul>

let format_ref ~org ~repo ~branch ~commit ~status =
  let url = commit_url ~org ~repo commit in
  let build_status = Build_status.class_name status in
  <li class=<%s build_status %>>
    <a href=<%s url %>><%s branch %></a>
  </li>

let refs_v ~org ~repo ~refs =
  let lis =
    let lis' = Client.Ref_map.bindings refs |> List.map @@ fun (branch, (commit, status)) ->
      format_ref ~org ~repo ~branch ~commit ~status
    in
      String.concat "" lis'
  in
  <ul class="statuses">
    <%s! lis %>
  </ul>

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
                      let branch = String.concat "/" branch in
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
                  | _ -> txt (Printf.sprintf "Bad ref format %S" r))
        @ [ txt ")" ])

let link_jobs ~org ~repo ~hash ?selected jobs =
  let open Tyxml.Html in
  let render_job trees { Client.variant; outcome } =
    let uri = job_url ~org ~repo ~hash variant in
    match List.rev (String.split_on_char Common.status_sep variant) with
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
  Template.instance @@ orgs_v ~orgs

let list_repos ~org ~repos =
  Template.instance @@ repos_v ~org ~repos

let list_refs ~org ~repo ~refs =
  Template.instance @@ refs_v ~org ~repo ~refs

let list_steps ~org ~repo ~refs ~hash ~jobs =
  Template_tyxml.instance [
        breadcrumbs ["github", "github";
                     org, org;
                     repo, repo] (short_hash hash);
        link_github_refs ~org ~repo refs;
        link_jobs ~org ~repo ~hash jobs;
      ]

let step_v ~org ~repo ~refs ~hash ~variant ~job chunk =
  let header, footer =
    let body = Template_tyxml.instance Tyxml.Html.[
        breadcrumbs ["github", "github";
                     org, org;
                     repo, repo;
                     short_hash hash, "commit/" ^ hash;
                    ] variant;
        link_github_refs ~org ~repo refs;
        pre [txt "@@@"]
  ] in
  Astring.String.cut ~sep:"@@@" body |> Option.get in
  let ansi = Ansi.create () in
  let open Lwt.Infix in
  Dream.stream
    ~headers:[ ("Content-type", "text/html; charset=utf-8") ]
    (fun response_stream ->
      let%lwt () = Dream.write response_stream header in
      let buffer = ref chunk in
      let rec loop () =
        match !buffer with
        | "", _ ->
          let%lwt () = Dream.write response_stream footer in
          Dream.close response_stream
        | data, next -> (
            let%lwt () = Dream.write response_stream (Ansi.process ansi data) in
            let%lwt () = Dream.flush response_stream in
            Current_rpc.Job.log job ~start:next >>= function
            | Ok x ->
                buffer := x;
                loop ()
            | Error (`Capnp ex) ->
                Dream.log "Error fetching logs: %a" Capnp_rpc.Error.pp ex;
                Dream.write response_stream
                  (Fmt.str "ocaml-ci error: %a@." Capnp_rpc.Error.pp ex))
      in
      loop ())

let show_step ~org ~repo ~refs ~hash ~variant job chunk =
  step_v ~org ~repo ~refs ~hash ~variant ~job chunk
