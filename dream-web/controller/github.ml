open Lwt.Infix
module Client = Ocaml_ci_api.Client
module Capability = Capnp_rpc_lwt.Capability

let not_found = Dream.respond ~status:`Not_Found View.Client_error.not_found

let ( >>!= ) x f =
  x >>= function
  | Error (`Capnp ex) ->
      Dream.respond ~status:`Internal_Server_Error
        (Fmt.to_to_string Capnp_rpc.Error.pp ex)
  | Ok y -> f y

let list_orgs ci =
  Client.CI.orgs ci >>!= fun orgs ->
  Dream.respond @@ View.Github.list_orgs ~orgs

let list_repos ~org ci =
  let list_repos' org o =
    Client.Org.repos o >>!= fun repos ->
    Dream.respond @@ View.Github.list_repos ~org ~repos
  in
  Capability.with_ref (Client.CI.org ci org) @@ list_repos' org

let list_refs ~org ~repo ci =
  Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
  Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
  Client.Repo.refs repo_cap >>!= fun refs ->
  Dream.respond @@ View.Github.list_refs ~org ~repo ~refs

let list_steps ~org ~repo ~hash ci =
  Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
  Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
  Capability.with_ref (Client.Repo.commit_of_hash repo_cap hash)
  @@ fun commit ->
  Client.Commit.jobs commit >>!= fun jobs ->
  Client.Commit.refs commit >>!= fun refs ->
  Dream.respond @@ View.Github.list_steps ~org ~repo ~refs ~hash ~jobs

let show_step ~org ~repo ~hash ~variant ci =
  Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
  Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
  Capability.with_ref (Client.Repo.commit_of_hash repo_cap hash)
  @@ fun commit ->
  let refs = Client.Commit.refs commit in
  let jobs = Client.Commit.jobs commit in
  Capability.with_ref (Client.Commit.job_of_variant commit variant)
  @@ fun job ->
  let status = Current_rpc.Job.status job in
  Current_rpc.Job.log job ~start:0L >>!= fun chunk ->
  (* (these will have resolved by now) *)
  refs >>!= fun refs ->
  jobs >>!= fun _jobs ->
  status >>!= fun _status ->
  View.Github.show_step ~org ~repo ~refs ~hash ~variant job chunk
