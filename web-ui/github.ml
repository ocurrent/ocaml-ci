open Lwt.Infix

module Capability = Capnp_rpc_lwt.Capability
module Client = Ocaml_ci_api.Client
module Common = Ocaml_ci_api.Common
module Server = Cohttp_lwt_unix.Server
module Response = Cohttp.Response.Make(Server.IO)
module Transfer_IO = Cohttp__Transfer_io.Make(Server.IO)

let headers = Cohttp.Header.init_with "Content-Type" "text/html; charset=utf-8"

let normal_response x =
  x >|= fun x -> `Response x

let respond_error status body =
  let headers = Cohttp.Header.init_with "Content-Type" "text/plain; charset=utf-8" in
  Server.respond_error ~status ~headers ~body () |> normal_response

let (>>!=) x f =
  x >>= function
  | Error `Capnp ex -> respond_error `Internal_server_error (Fmt.to_to_string Capnp_rpc.Error.pp ex)
  | Ok y -> f y

let org_url owner =
  Printf.sprintf "/github/%s" owner

let repo_url ~owner name =
  Printf.sprintf "/github/%s/%s" owner name

let job_url ~owner ~name ~hash variant =
  Printf.sprintf "/github/%s/%s/commit/%s/variant/%s" owner name hash variant

let commit_url ~owner ~name hash =
  Printf.sprintf "/github/%s/%s/commit/%s" owner name hash

let github_branch_url ~owner ~name ref =
  Printf.sprintf "https://github.com/%s/%s/tree/%s" owner name ref

let github_pr_url ~owner ~name id =
  Printf.sprintf "https://github.com/%s/%s/pull/%s" owner name id

let breadcrumbs steps page_title =
  let open Tyxml.Html in
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

module StatusTree : sig
  type key = string

  type 'a tree =
    | Leaf of 'a
    | Branch of key * 'a t
  and 'a t = 'a tree list

  val add : key list -> 'a -> 'a t -> 'a t
end = struct
  type key = string

  type 'a tree =
    | Leaf of 'a
    | Branch of key * 'a t
  and 'a t = 'a tree list

  let rec add k x ts = match k, ts with
    | [], ts -> ts @ [Leaf x]
    | k::ks, [] -> [Branch (k, add ks x [])]
    | _::_, (Leaf _ as t)::ts -> t :: add k x ts
    | k::ks, Branch (k', t)::ts when String.equal k k' -> Branch (k, add ks x t) :: ts
    | _::_, (Branch _ as t)::ts -> t :: add k x ts
end

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

let statuses ss =
  let open Tyxml.Html in
  let rec render_status = function
    | StatusTree.Leaf (s, elms) ->
        let status_class_name =
          match (s : Client.State.t) with
          | NotStarted -> "not-started"
          | Aborted -> "aborted"
          | Failed m when Astring.String.is_prefix ~affix:"[SKIP]" m -> "skipped"
          | Failed _ -> "failed"
          | Passed -> "passed"
          | Active -> "active"
          | Undefined _ -> "undefined"
        in
        li ~a:[a_class [status_class_name]] elms
    | StatusTree.Branch (b, ss) ->
        li [txt b; ul ~a:[a_class ["statuses"]] (List.map render_status ss)]
  in
  ul ~a:[a_class ["statuses"]] (List.map render_status ss)

let format_refs ~owner ~name refs =
  let open Tyxml.Html in
  ul ~a:[a_class ["statuses"]] (
    Client.Ref_map.bindings refs |> List.map @@ fun (branch, (commit, status)) ->
    li ~a:[a_class [Build_status.class_name status]] [
      a ~a:[a_href (commit_url ~owner ~name commit)] [txt branch]
    ]
  )

let rec intersperse ~sep = function
  | [] -> []
  | [x] -> [x]
  | x :: xs -> x :: sep :: intersperse ~sep xs

let link_github_refs ~owner ~name =
  let open Tyxml.Html in
  function
  | [] -> txt "(not at the head of any monitored branch or PR)"
  | refs ->
    p (
      txt "(for " ::
      (
        intersperse ~sep:(txt ", ") (
          refs |> List.map @@ fun r ->
          match Astring.String.cuts ~sep:"/" r with
          | "refs"::"heads"::branch ->
            let branch = String.concat "/" branch in
            span [txt "branch "; a ~a:[a_href (github_branch_url ~owner ~name branch)] [ txt branch ]]
          | ["refs"; "pull"; id; "head"] ->
            span [txt "PR "; a ~a:[a_href (github_pr_url ~owner ~name id)] [ txt ("#" ^ id) ]]
          | _ ->
            txt (Printf.sprintf "Bad ref format %S" r)
        )
      ) @
      [txt ")"]
    )

let link_jobs ~owner ~name ~hash ?selected jobs =
  let open Tyxml.Html in
  let render_job trees { Client.variant; outcome } =
    let uri = job_url ~owner ~name ~hash variant in
    match List.rev (String.split_on_char Common.status_sep variant) with
    | [] -> assert false
    | label_txt::k ->
        let k = List.rev k in
        let x =
          let label = txt (Fmt.str "%s (%a)" label_txt Client.State.pp outcome) in
          let label = if selected = Some variant then b [label] else label in
          outcome, [a ~a:[a_href uri] [label]]
        in
        StatusTree.add k x trees
  in
  statuses (List.fold_left render_job [] jobs)

let short_hash = Astring.String.with_range ~len:6

let stream_logs job ~owner ~name ~refs ~hash ~jobs ~variant ~status (data, next) writer =
  let header, footer =
    let can_rebuild = status.Current_rpc.Job.can_rebuild in
    let buttons =
      if can_rebuild then Tyxml.Html.[
          form ~a:[a_action (variant ^ "/rebuild"); a_method `Post] [
            input ~a:[a_input_type `Submit; a_value "Rebuild"] ()
          ]
      ] else []
    in
    let body = Template.instance Tyxml.Html.[
        breadcrumbs ["github", "github";
                     owner, owner;
                     name, name;
                     short_hash hash, "commit/" ^ hash;
                    ] variant;
        link_github_refs ~owner ~name refs;
        link_jobs ~owner ~name ~hash ~selected:variant jobs;
        div buttons;
        pre [txt "@@@"]
      ] in
    Astring.String.cut ~sep:"@@@" body |> Option.get
  in
  let ansi = Ansi.create () in
  Transfer_IO.write writer (header ^ (Ansi.process ansi data)) >>= fun () ->
  let rec aux next =
    Current_rpc.Job.log job ~start:next >>= function
    | Ok ("", _) ->
      Transfer_IO.write writer footer
    | Ok (data, next) ->
      Transfer_IO.write writer (Ansi.process ansi data) >>= fun () ->
      aux next
    | Error (`Capnp ex) ->
      Log.warn (fun f -> f "Error fetching logs: %a" Capnp_rpc.Error.pp ex);
      Transfer_IO.write writer (Fmt.str "ocaml-ci error: %a@." Capnp_rpc.Error.pp ex)
  in
  aux next

let repo_handle ~meth ~owner ~name ~repo path =
  match meth, path with
  | `GET, [] ->
      Client.Repo.refs repo >>!= fun refs ->
      let body = Template.instance [
          breadcrumbs ["github", "github";
                       owner, owner] name;
          format_refs ~owner ~name refs
        ] in
      Server.respond_string ~status:`OK ~headers ~body () |> normal_response
  | `GET, ["commit"; hash] ->
    Capability.with_ref (Client.Repo.commit_of_hash repo hash) @@ fun commit ->
    let refs = Client.Commit.refs commit in
    Client.Commit.jobs commit >>!= fun jobs ->
    refs >>!= fun refs ->
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
      if can_cancel then Tyxml.Html.[
          form ~a:[a_action (hash ^ "/cancel"); a_method `Post] [
            input ~a:[a_input_type `Submit; a_value "Cancel"] ()
          ]
      ] else if can_rebuild then Tyxml.Html.[
        form ~a:[a_action (hash ^ "/rebuild-failed"); a_method `Post] [
          button [txt "Rebuild Failed"];
          input ~a:[a_name "filter"; a_input_type `Hidden; a_value "failed"] ()
        ];
        form ~a:[a_action (hash ^ "/rebuild-all"); a_method `Post] [
          button [txt "Rebuild All"];
          input ~a:[a_name "filter"; a_input_type `Hidden; a_value "none"] ()
        ];
      ] else []
    in
    let body = Template.instance Tyxml.Html.[
        breadcrumbs ["github", "github";
                     owner, owner;
                     name, name] (short_hash hash);
        link_github_refs ~owner ~name refs;
        link_jobs ~owner ~name ~hash jobs;
        div buttons;
      ] in
    Server.respond_string ~status:`OK ~headers ~body () |> normal_response
  | `GET, ["commit"; hash; "variant"; variant] ->
    Capability.with_ref (Client.Repo.commit_of_hash repo hash) @@ fun commit ->
    let refs = Client.Commit.refs commit in
    let jobs = Client.Commit.jobs commit in
    Capability.with_ref (Client.Commit.job_of_variant commit variant) @@ fun job ->
    let status = Current_rpc.Job.status job in
    Current_rpc.Job.log job ~start:0L >>!= fun chunk ->
    (* (these will have resolved by now) *)
    refs >>!= fun refs ->
    jobs >>!= fun jobs ->
    status >>!= fun status ->
    let headers =
      (* Otherwise, an nginx reverse proxy will wait for the whole log before sending anything. *)
      Cohttp.Header.add headers "X-Accel-Buffering" "no"
    in
    let res = Cohttp.Response.make ~status:`OK ~flush:true ~encoding:Cohttp.Transfer.Chunked ~headers () in
    let write _ic oc =
      let flush = Cohttp.Response.flush res in
      let writer = Transfer_IO.make_writer ~flush Cohttp.Transfer.Chunked oc in
      Lwt.finalize
        (fun () ->
           stream_logs job ~owner ~name ~refs ~hash ~jobs ~variant ~status chunk writer >>= fun () ->
           Server.IO.write oc "0\r\n\r\n"
        )
        (fun () ->
           Capability.dec_ref job;
           Lwt.return_unit
        )
    in
    Capability.inc_ref job;
    Lwt.return (`Expert (res, write))
  | `POST, ["commit"; hash; "variant"; variant; "rebuild"] ->
    Capability.with_ref (Client.Repo.commit_of_hash repo hash) @@ fun commit ->
    Capability.with_ref (Client.Commit.job_of_variant commit variant) @@ fun job ->
    Capability.with_ref (Current_rpc.Job.rebuild job) @@ fun new_job ->
    begin Capability.await_settled new_job >>= function
      | Ok () ->
        let uri = job_url ~owner ~name ~hash variant |> Uri.of_string in
        Server.respond_redirect ~uri () |> normal_response
      | Error { Capnp_rpc.Exception.reason; _ } ->
        respond_error `Internal_server_error reason
    end
  | `POST, ["commit"; hash; "cancel"] ->
    let job_lookup commit job_info =
      let variant = job_info.Client.variant in
      let job = Client.Commit.job_of_variant commit variant in
      Current_rpc.Job.status job |> Lwt.map (function
        | Ok s -> if s.Current_rpc.Job.can_cancel then Some (job_info, job) else None
        | Error (`Capnp ex) -> Log.info (fun f -> f "Error fetching job status: %a" Capnp_rpc.Error.pp ex); None
      )
    in
    let can_cancel (commit: Client.Commit.t) (job_i: Client.job_info) =
      match job_i.outcome with
      | Active | NotStarted ->
          job_lookup commit job_i
      | Aborted | Failed _ | Passed | Undefined _ -> Lwt.return None
    in
    let cancel_many (commit: Client.Commit.t) (job_infos : Client.job_info list) =
      let open Lwt.Infix in
      Lwt_list.filter_map_p (fun job_info -> can_cancel commit job_info) job_infos >>= fun l ->
        Lwt_list.map_p (fun (ji, j) -> Current_rpc.Job.cancel j |> Lwt_result.map @@ fun () -> ji) l >>= fun x ->
          let success = ref [] in
          let failed = ref 0 in
          let log_error r =
          match r with
          | Ok(ji) -> success := ji :: !success
          | Error (`Capnp ex) ->
            incr failed;
            Log.err (fun f -> f "Error cancelling job: %a" Capnp_rpc.Error.pp ex)
          in
          let _ = List.map log_error x in
          Lwt.return (!success, !failed)
    in
    Capability.with_ref (Client.Repo.commit_of_hash repo hash) @@ fun commit ->
    let refs = Client.Commit.refs commit in
    refs >>!= fun refs ->
    Client.Commit.jobs commit >>!= fun jobs ->
      begin cancel_many commit jobs >>= fun (success, failed) ->
        let open Tyxml.Html in
        let uri = commit_url ~owner ~name hash in
        let format_job_info ji =
          li [span [txt @@ Fmt.str "Cancelling job: %s" ji.Client.variant]]
        in
        let success_msg =
          if List.length success > 0
          then ul (List.map format_job_info success)
          else div [span [txt @@ Fmt.str "No jobs were cancelled."]]
        in
        let fail_msg = match failed with
        | n when n <= 0 -> div []
        | 1 ->  div [span [txt @@ Fmt.str "1 job could not be cancelled. Check logs for more detail."]]
        | n ->  div [span [txt @@ Fmt.str "%d jobs could not be cancelled. Check logs for more detail." n]]
        in
        let return_link =
          a ~a:[a_href (uri)] [txt @@ Fmt.str "Return to %s" (short_hash hash)]
        in
        let body = Template.instance [
          breadcrumbs ["github", "github";
                      owner, owner;
                      name, name] (short_hash hash);
          link_github_refs ~owner ~name refs;
          link_jobs ~owner ~name ~hash jobs;
          success_msg;
          fail_msg;
          return_link;
          ] in
        Server.respond_string ~status:`OK ~headers ~body () |> normal_response
    end
  | `POST, ["commit"; hash; "rebuild-all"] | `POST, ["commit"; hash; "rebuild-failed"] ->
    let rebuild_failed_only =
      match path with
      | ["commit";_;"rebuild-all"] -> false
      | ["commit";_;"rebuild-failed"] -> true
      | _ -> false
    in
    let job_lookup commit job_info status_check =
      let variant = job_info.Client.variant in
      let job = Client.Commit.job_of_variant commit variant in
      Current_rpc.Job.status job |> Lwt.map (function
        | Ok s -> if status_check s then Some (job_info, job) else None
        | Error (`Capnp ex) -> Log.info (fun f -> f "Error fetching job status: %a" Capnp_rpc.Error.pp ex); None
      )
    in
    let can_rebuild (commit: Client.Commit.t) (job_i: Client.job_info) =
      if rebuild_failed_only then
        match job_i.outcome with
        | Active | NotStarted | Passed -> Lwt.return None
        | Aborted | Failed _ | Undefined _ -> job_lookup commit job_i (fun s -> s.Current_rpc.Job.can_rebuild)
      else
        job_lookup commit job_i (fun s -> s.Current_rpc.Job.can_rebuild)
    in
    let success = ref [] in
    let failed = ref 0 in
    let rebuild_many commit job_infos =
      Lwt_list.filter_map_p (fun job_info -> can_rebuild commit job_info) job_infos >>=
      Lwt_list.map_p (fun (ji, j) ->
          Capability.with_ref (Current_rpc.Job.rebuild j) @@ fun new_job ->
            begin Capability.await_settled new_job >>= function
            | Ok () -> success := ji :: !success; Lwt.return_unit
            | Error ex ->
              incr failed;
              Log.err (fun f -> f "Error rebuilding job: %a" Capnp_rpc.Exception.pp ex);
              Lwt.return_unit
            end
        )
    in
    Capability.with_ref (Client.Repo.commit_of_hash repo hash) @@ fun commit ->
      let refs = Client.Commit.refs commit in
      refs >>!= fun refs ->
      Client.Commit.jobs commit >>!= fun jobs ->
        begin rebuild_many commit jobs >>= fun _unit ->
          let open Tyxml.Html in
          let uri = commit_url ~owner ~name hash in
          let format_job_info ji =
            li [span [txt @@ Fmt.str "Rebuilding job: %s" ji.Client.variant]]
          in
          let success_msg =
            if List.length !success > 0
            then ul (List.map format_job_info !success)
            else div [span [txt @@ Fmt.str "No jobs were rebuilt."]]
          in
          let fail_msg = match !failed with
          | n when n <= 0 -> div []
          | 1 ->  div [span [txt @@ Fmt.str "1 job could not be rebuilt. Check logs for more detail."]]
          | n ->  div [span [txt @@ Fmt.str "%d jobs could not be rebuilt. Check logs for more detail." n]]
          in
          let return_link =
            a ~a:[a_href (uri)] [txt @@ Fmt.str "Return to %s" (short_hash hash)]
          in
          let body = Template.instance [
            breadcrumbs ["github", "github";
                        owner, owner;
                        name, name] (short_hash hash);
            link_github_refs ~owner ~name refs;
            link_jobs ~owner ~name ~hash jobs;
            success_msg;
            fail_msg;
            return_link;
          ] in
        Server.respond_string ~status:`OK ~headers ~body () |> normal_response
      end
  | _ ->
    Server.respond_not_found () |> normal_response

let format_org org =
  let open Tyxml.Html in
  li [a ~a:[a_href (org_url org)] [txt org]]

let list_orgs ci =
  Client.CI.orgs ci >>!= fun orgs ->
  let body = Template.instance Tyxml.Html.[
      breadcrumbs [] "github";
      ul (List.map format_org orgs)
    ] in
  Server.respond_string ~status:`OK ~headers ~body () |> normal_response

let format_repo ~owner { Client.Org.name; master_status } =
  let open Tyxml.Html in
  li ~a:[a_class [Build_status.class_name master_status]] [
    a ~a:[a_href (repo_url ~owner name)] [txt name]
  ]

let list_repos ~owner org =
  Client.Org.repos org >>!= fun repos ->
  let body = Template.instance Tyxml.Html.[
      breadcrumbs ["github", "github"] owner;
      ul ~a:[a_class ["statuses"]] (List.map (format_repo ~owner) repos)
    ] in
  Server.respond_string ~status:`OK ~headers ~body () |> normal_response

let handle ~backend ~meth path =
  Backend.ci backend >>= fun ci ->
  match meth, path with
  | `GET, [] -> list_orgs ci
  | `GET, [owner] -> Capability.with_ref (Client.CI.org ci owner) @@ list_repos ~owner
  | meth, (owner :: name :: path) ->
    Capability.with_ref (Client.CI.org ci owner) @@ fun org ->
    Capability.with_ref (Client.Org.repo org name) @@ fun repo ->
    repo_handle ~meth ~owner ~name ~repo path
  | _ ->
    Server.respond_not_found () |> normal_response
