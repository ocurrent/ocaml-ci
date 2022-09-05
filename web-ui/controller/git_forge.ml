module type View = View.Git_forge.View

module Client = Ocaml_ci_api.Client
module Run_time = Ocaml_ci_client_lib.Run_time

module type Controller = sig
  val list_orgs : Backend.t -> Dream.response Lwt.t
  val list_repos : org:string -> Backend.t -> Dream.server Dream.message Lwt.t

  val list_refs :
    org:string -> repo:string -> Backend.t -> Dream.server Dream.message Lwt.t

  val list_steps :
    org:string ->
    repo:string ->
    hash:string ->
    Dream.request ->
    Backend.t ->
    Dream.response Lwt.t

  val show_step :
    org:string ->
    repo:string ->
    hash:string ->
    variant:string ->
    Dream.client Dream.message ->
    Backend.t ->
    Dream.server Dream.message Lwt.t

  val rebuild_step :
    org:string ->
    repo:string ->
    hash:string ->
    variant:string ->
    Dream.client Dream.message ->
    Backend.t ->
    Dream.server Dream.message Lwt.t

  val cancel_steps :
    org:string ->
    repo:string ->
    hash:string ->
    Dream.request ->
    Backend.t ->
    Dream.response Lwt.t

  val rebuild_steps :
    rebuild_failed_only:bool ->
    org:string ->
    repo:string ->
    hash:string ->
    Dream.client Dream.message ->
    Backend.t ->
    Dream.server Dream.message Lwt.t
end

(* Abstract controller for any Git_forge that implements `Ocaml_ci_api.Client` API *)
module Make (View : View) = struct
  open Lwt.Infix
  module Client = Ocaml_ci_api.Client
  module Capability = Capnp_rpc_lwt.Capability

  let ( >>!= ) x f =
    x >>= function
    | Error (`Capnp ex) ->
        Dream.log "Internal server error: %s"
          (Fmt.to_to_string Capnp_rpc.Error.pp ex);
        Dream.empty `Internal_Server_Error
    | Ok y -> f y

  let ( >>!!= ) x f =
    x >>= function
    | Ok y -> f y
    | v -> (
        match v with
        | Error (`Capnp ex) ->
            Dream.log "Internal server error: %s"
              (Fmt.to_to_string Capnp_rpc.Error.pp ex);
            Dream.empty `Internal_Server_Error
        | _ ->
            (* FIXME: This is trying to match `Msg of variant *)
            Dream.empty `Internal_Server_Error)

  let job_url ~org ~repo ~hash variant =
    Fmt.str "/%s/%s/%s/commit/%s/variant/%s" View.prefix org repo hash variant

  let list_orgs ci =
    Backend.ci ci >>= Client.CI.orgs >>!= fun orgs ->
    Dream.respond @@ View.list_orgs ~orgs

  let list_repos ~org ci =
    Backend.ci ci >>= fun ci ->
    Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
    Client.Org.repos org_cap >>!= fun repos ->
    Dream.respond @@ View.list_repos ~org ~repos

  let list_refs ~org ~repo ci =
    Backend.ci ci >>= fun ci ->
    Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
    Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
    Client.Repo.refs repo_cap >>!= fun refs ->
    Dream.respond @@ View.list_refs ~org ~repo ~refs

  let list_steps ~org ~repo ~hash request ci =
    Backend.ci ci >>= fun ci ->
    Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
    Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
    Capability.with_ref (Client.Repo.commit_of_hash repo_cap hash)
    @@ fun commit_cap ->
    Client.Commit.status commit_cap >>!!= fun status ->
    Client.Commit.jobs commit_cap >>!= fun jobs ->
    Client.Commit.refs commit_cap >>!= fun refs ->
    let build_status : Client.State.t =
      match status with
      | `Failed -> Failed ""
      | `Pending | `Not_started -> NotStarted
      | `Passed -> Passed
    in
    let csrf_token = Dream.csrf_tag request in
    let flash_messages = Dream.flash_messages request in
    let first_step_queued_at =
      match Run_time.first_step_queued_at jobs with
      | Error e ->
          Dream.log "Error - %s" e;
          None
      | Ok v -> Some v
    in
    let total_run_time = Run_time.total_of_run_times jobs in
    Dream.respond
    @@ View.list_steps ~org ~repo ~refs ~hash ~jobs ~csrf_token
         ~first_step_queued_at ~total_run_time ~flash_messages ~build_status ()

  let show_step ~org ~repo ~hash ~variant request ci =
    Backend.ci ci >>= fun ci ->
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
    let build_created_at =
      Option.join @@ Result.to_option @@ Run_time.build_created_at ~build:jobs
    in
    let step_info =
      let filter (j : Client.job_info) = j.variant = variant in
      List.find_opt filter jobs
    in
    let timestamps = Option.map Run_time.timestamps_from_job_info step_info in
    let timestamps =
      match timestamps with
      | None ->
          Dream.log "Error - No step-info.";
          None
      | Some (Error e) ->
          Dream.log "Error - %s" e;
          None
      | Some (Ok t) -> Some t
    in
    View.show_step ~org ~repo ~refs ~hash ~jobs ~variant ~status ~csrf_token
      ~flash_messages ~timestamps ~build_created_at ~job:job_cap chunk

  let rebuild_step ~org ~repo ~hash ~variant request ci =
    Backend.ci ci >>= fun ci ->
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
    Backend.ci ci >>= fun ci ->
    let cancel_many (commit : Client.Commit.t)
        (job_infos : Client.job_info list) =
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
    let success_msg = View.cancel_success_message success in
    let fail_msg = View.cancel_fail_message failed in
    let return_link = View.return_link ~org ~repo ~hash in
    let csrf_token = Dream.csrf_tag request in
    let first_step_queued_at =
      match Run_time.first_step_queued_at jobs with
      | Error e ->
          Dream.log "Error - %s" e;
          None
      | Ok v -> Some v
    in
    let total_run_time = Run_time.total_of_run_times jobs in
    Dream.respond
    @@ View.list_steps ~org ~repo ~refs ~hash ~jobs ~success_msg ~fail_msg
         ~return_link ~csrf_token ~first_step_queued_at ~total_run_time ()

  let rebuild_steps ~rebuild_failed_only ~org ~repo ~hash request ci =
    Backend.ci ci >>= fun ci ->
    let rebuild_many commit job_infos =
      let go job_info commit success failed =
        let variant = job_info.Client.variant in
        if variant = "(analysis)" then
          (* Do not rebuild analysis -- this triggers other jobs *)
          Lwt.return (success, failed)
        else
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
          | Aborted | Failed _ | Undefined _ ->
              go job_info commit success failed
        else go job_info commit success failed
      in
      Lwt_list.fold_left_s f init job_infos
    in
    Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
    Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
    Capability.with_ref (Client.Repo.commit_of_hash repo_cap hash)
    @@ fun commit_cap ->
    Client.Commit.refs commit_cap >>!= fun refs ->
    Client.Commit.jobs commit_cap >>!= fun jobs ->
    rebuild_many commit_cap jobs >>= fun (success, failed) ->
    let success_msg = View.rebuild_success_message success in
    let fail_msg = View.rebuild_fail_message failed in
    let return_link = View.return_link ~org ~repo ~hash in
    let csrf_token = Dream.csrf_tag request in
    let first_step_queued_at =
      match Run_time.first_step_queued_at jobs with
      | Error e ->
          Dream.log "Error - %s" e;
          None
      | Ok v -> Some v
    in
    let total_run_time = Run_time.total_of_run_times jobs in
    Dream.respond
    @@ View.list_steps ~org ~repo ~refs ~hash ~jobs ~success_msg ~fail_msg
         ~return_link ~csrf_token ~first_step_queued_at ~total_run_time ()
end
