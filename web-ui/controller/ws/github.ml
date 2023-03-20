open Lwt.Infix
module Client = Ocaml_ci_api.Client
module Capability = Capnp_rpc_lwt.Capability

let ( >>!= ) = Controller.Git_forge.( >>!= )

let show_step ~org ~repo ~hash ~variant _request ci =
  Controller.Backend.ci ci >>= fun ci ->
  Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
  Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
  Capability.with_ref (Client.Repo.commit_of_hash repo_cap hash)
  @@ fun commit_cap ->
  let refs = Client.Commit.refs commit_cap in
  let jobs = Client.Commit.jobs commit_cap in
  Capability.with_ref (Client.Commit.job_of_variant commit_cap variant)
  @@ fun job_cap ->
  let status = Current_rpc.Job.status job_cap in
  Current_rpc.Job.log job_cap ~start:0L >>!= fun (data, next) ->
  (* (these will have resolved by now) *)
  refs >>!= fun _refs ->
  jobs >>!= fun _jobs ->
  status >>!= fun _status ->
  Capability.inc_ref job_cap;

  Dream.websocket (fun websocket ->
      Dream.send websocket data >>= fun () ->
      let rec loop next =
        Current_rpc.Job.log job_cap ~start:next >>= function
        | Ok ("", _) -> Dream.close_websocket websocket
        | Ok (data, next) -> Dream.send websocket data >>= fun () -> loop next
        | Error (`Capnp ex) ->
            Dream.log "Error fetching logs: %a" Capnp_rpc.Error.pp ex;
            Dream.close_websocket websocket
      in
      loop next)
