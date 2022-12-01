module type Api = Api_view.Git_forge.Api

module type Api_controller = sig
  val list_repos : org:string -> Controller.Backend.t -> Dream.response Lwt.t

  val list_steps :
    org:string ->
    repo:string ->
    hash:string ->
    Controller.Backend.t ->
    Dream.response Lwt.t

  val show_step :
    org:string ->
    repo:string ->
    hash:string ->
    variant:string ->
    Controller.Backend.t ->
    Dream.response Lwt.t
end

module Make (Api : Api) = struct
  open Lwt.Infix
  module Client = Ocaml_ci_api.Client
  module Capability = Capnp_rpc_lwt.Capability
  module Run_time = Ocaml_ci_client_lib.Run_time

  let ( >>!= ) = Controller.Git_forge.( >>!= )

  let show_step ~org ~repo ~hash ~variant ci =
    Controller.Backend.ci ci >>= fun ci ->
    Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
    Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
    Capability.with_ref (Client.Repo.commit_of_hash repo_cap hash)
    @@ fun commit_cap ->
    let jobs = Client.Commit.jobs commit_cap in
    Capability.with_ref (Client.Commit.job_of_variant commit_cap variant)
    @@ fun job_cap ->
    let status = Current_rpc.Job.status job_cap in
    Current_rpc.Job.log job_cap ~start:0L >>!= fun _ ->
    jobs >>!= fun jobs ->
    status >>!= fun status ->
    Capability.inc_ref job_cap;
    let build_created_at =
      Run_time.build_created_at ~build:jobs
      |> Result.to_option
      |> Option.join
      |> Option.value ~default:0.
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
    let run_time =
      Option.map
        (Run_time.run_times_from_timestamps ~build_created_at)
        timestamps
    in
    Api.show_step ~step_info ~run_time ~can_rebuild:status.can_rebuild

  let list_steps ~org ~repo ~hash ci =
    Controller.Backend.ci ci >>= fun ci ->
    Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
    Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
    Capability.with_ref (Client.Repo.commit_of_hash repo_cap hash)
    @@ fun commit_cap ->
    Client.Commit.status commit_cap >>!= fun status ->
    Client.Commit.jobs commit_cap >>!= fun jobs ->
    let build_status = Client.State.from_build_status status in
    Api.list_steps ~org ~repo ~hash ~build_status ~jobs

  let join xs f : Dream.response Lwt.t =
    let to_option = function
      | Error (`Capnp ex) ->
          Dream.log "Internal server error: %s"
            (Fmt.to_to_string Capnp_rpc.Error.pp ex);
          None
      | Error (`Msg v) ->
          Dream.log "Internal server error: %s" v;
          None
      | Ok y -> Some y
    in
    let xs = List.map to_option xs in
    if List.exists Option.is_none xs then Dream.empty `Internal_Server_Error
    else f (List.filter_map Fun.id xs)

  let list_repos ~org ci =
    Controller.Backend.ci ci >>= fun ci ->
    Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
    Client.Org.repos org_cap >>!= fun repos ->
    (* let rec run_with_repo_caps caps_acc f = function
         | [] -> f caps_acc
         | repo :: xs ->
             Capability.with_ref (Client.Org.repo org_cap repo.Client.Org.name)
             @@ fun repo_cap -> run_with_repo_caps (repo_cap :: caps_acc) f xs
       in
       let capabilities f =
         List.map
           (fun repo ->
             Capability.with_ref (Client.Org.repo org_cap repo.Client.Org.name) f)
           repos
       in
       let repo_caps =
         let default = Client.Repo.default_ref repo_cap in
         Client.Repo.history_of_ref repo_cap default.gref
       in
       let repos = List.map (fun repo -> (repo, get_history repo)) repos in
       Api.list_repos ~repos *)
    ignore repos;
    let repos =
      [
        ( "cavalry",
          [
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Failed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 5.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 15.;
              };
          ] );
        ( "Otorus",
          [
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Failed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 5.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 15.;
              };
          ] );
        ( "ci-test-repo",
          [
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Failed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 20.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Failed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 5.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 10.;
              };
            Client.Repo.
              {
                gref = "";
                hash = "";
                status = Passed;
                started_at = Some 0.;
                message = "";
                name = "";
                ran_for = Some 15.;
              };
          ] );
      ]
    in
    Api.list_repos ~repos
end
