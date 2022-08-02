open Lwt.Infix
module Client = Ocaml_ci_api.Client
module Capability = Capnp_rpc_lwt.Capability

let ( >>!= ) x f =
  x >>= function
  | Error (`Capnp ex) ->
    Dream.log "Internal server error: %s" (Fmt.to_to_string Capnp_rpc.Error.pp ex);
    Dream.empty `Internal_Server_Error
  | Ok y -> f y

(*  job is synonymous with step.
    'step' is used in a user-facing context.
    'job' is an internal concept. *)
let job_url ~org ~repo ~hash variant =
  Fmt.str "/github/%s/%s/commit/%s/variant/%s" org repo hash variant

let list_orgs ci =
  Client.CI.orgs ci >>!= fun orgs ->
  Dream.respond @@ View.Github.list_orgs ~orgs

let list_repos ~org ci =
  Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
  Client.Org.repos org_cap >>!= fun repos ->
  Dream.respond @@ View.Github.list_repos ~org ~repos

let list_refs ~org ~repo ci =
  Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
  Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
  Client.Repo.refs repo_cap >>!= fun refs ->
  Dream.respond @@ View.Github.list_refs ~org ~repo ~refs

let list_steps ~org ~repo ~hash request ci =
  Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
  Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
  Capability.with_ref (Client.Repo.commit_of_hash repo_cap hash)
  @@ fun commit ->
  Client.Commit.jobs commit >>!= fun jobs ->
  Client.Commit.refs commit >>!= fun refs ->
  let csrf_token = Dream.csrf_tag request in
  let flash_messages = Dream.flash_messages request in
  Dream.respond
  @@ View.Github.list_steps ~org ~repo ~refs ~hash ~jobs ~csrf_token
       ~flash_messages ()

let show_step ~org ~repo ~hash ~variant request ci =
  Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
  Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
  Capability.with_ref (Client.Repo.commit_of_hash repo_cap hash)
  @@ fun commit_cap ->
  let refs = Client.Commit.refs commit_cap in
  let jobs = Client.Commit.jobs commit_cap in
  Capability.with_ref (Client.Commit.job_of_variant commit_cap variant)
  @@ fun job_cap ->
  let status = Current_rpc.Job.status job_cap in
  Current_rpc.Job.log job_cap ~start:0L >>!= fun chunk ->
  (* (these will have resolved by now) *)
  refs >>!= fun refs ->
  jobs >>!= fun jobs ->
  status >>!= fun status ->
  Capability.inc_ref job_cap;
  let csrf_token = Dream.csrf_tag request in
  let flash_messages = Dream.flash_messages request in
  View.Github.show_step ~org ~repo ~refs ~hash ~jobs ~variant ~status
    ~csrf_token ~flash_messages ~job:job_cap chunk

let rebuild_step ~org ~repo ~hash ~variant request ci =
  Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
  Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
  Capability.with_ref (Client.Repo.commit_of_hash repo_cap hash)
  @@ fun commit_cap ->
  Capability.with_ref (Client.Commit.job_of_variant commit_cap variant)
  @@ fun job_cap ->
  Capability.with_ref (Current_rpc.Job.rebuild job_cap) @@ fun new_job_cap ->
  Capability.await_settled new_job_cap >>= function
  | Ok () ->
      let () = Dream.add_flash_message request "Rebuilding" variant in
      let uri = job_url ~org ~repo ~hash variant in
      Dream.redirect request uri
  | Error { Capnp_rpc.Exception.reason; _ } ->
      Dream.log "Internal server error: %s" reason;
      Dream.empty `Internal_Server_Error

let cancel_steps ~org ~repo ~hash request ci =
  let cancel_many (commit : Client.Commit.t) (job_infos : Client.job_info list) =
   let init = ([], 0) in
     let f (success, failed) (job_info : Client.job_info) =
            match job_info.outcome with
            | Aborted | Failed _ | Passed | Undefined _ ->
                Lwt.return (success, failed)
            | Active | NotStarted -> (
                let variant = job_info.Client.variant in
                let job = Client.Commit.job_of_variant commit variant in
                Current_rpc.Job.cancel job >|= function
                | Ok () -> (job_info :: success, failed)
                | Error (`Capnp ex) ->
                    Dream.log "Error cancelling job: %a" Capnp_rpc.Error.pp ex;
                    (success, succ failed))
    in
    Lwt_list.fold_left_s f init job_infos
    in
  Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
  Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
  Capability.with_ref (Client.Repo.commit_of_hash repo_cap hash)
  @@ fun commit_cap ->
  Client.Commit.refs commit_cap >>!= fun refs ->
  Client.Commit.jobs commit_cap >>!= fun jobs ->
  cancel_many commit_cap jobs >>= fun (success, failed) ->
  let success_msg = View.Github.cancel_success_message success in
  let fail_msg = View.Github.cancel_fail_message failed in
  let return_link = View.Github.return_link ~org ~repo ~hash in
  let csrf_token = Dream.csrf_tag request in
  Dream.respond
  @@ View.Github.list_steps ~org ~repo ~refs ~hash ~jobs ~success_msg ~fail_msg
       ~return_link ~csrf_token ()

let rebuild_steps ~rebuild_failed_only ~org ~repo ~hash request ci =
  let rebuild_many commit job_infos =
    let go job_info commit success failed =
      let variant = job_info.Client.variant in
      let job = Client.Commit.job_of_variant commit variant in
      Capability.with_ref (Current_rpc.Job.rebuild job) @@ fun new_job ->
      Capability.await_settled new_job >>= function
      | Ok () -> Lwt.return (job_info :: success, failed)
      | Error ex ->
          Dream.log "Error rebuilding job: %a" Capnp_rpc.Exception.pp ex;
          Lwt.return (success, succ failed)
    in
    let init = ([], 0) in
        let f (success, failed) (job_info : Client.job_info) =
        if rebuild_failed_only then
          match job_info.outcome with
          | Active | NotStarted | Passed -> Lwt.return (success, failed)
          | Aborted | Failed _ | Undefined _ -> go job_info commit success failed
        else go job_info commit success failed
    in
    Lwt_list.fold_left_s f init  job_infos
  in
  Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
  Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
  Capability.with_ref (Client.Repo.commit_of_hash repo_cap hash)
  @@ fun commit_cap ->
  Client.Commit.refs commit_cap >>!= fun refs ->
  Client.Commit.jobs commit_cap >>!= fun jobs ->
  rebuild_many commit_cap jobs >>= fun (success, failed) ->
  let success_msg = View.Github.rebuild_success_message success in
  let fail_msg = View.Github.rebuild_fail_message failed in
  let return_link = View.Github.return_link ~org ~repo ~hash in
  let csrf_token = Dream.csrf_tag request in
  Dream.respond
  @@ View.Github.list_steps ~org ~repo ~refs ~hash ~jobs ~success_msg ~fail_msg
       ~return_link ~csrf_token ()
