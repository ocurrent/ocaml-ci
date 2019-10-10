open Lwt.Infix
open Capnp_rpc_lwt

type git_ref = string
type git_hash = string

module Ref_map = Map.Make(String)

module CI = struct
  type t = Raw.Client.CI.t Capability.t

  let org t owner =
    let open Raw.Client.CI.Org in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.owner_set params owner;
    Capability.call_for_caps t method_id request Results.org_get_pipelined

  let orgs t =
    let open Raw.Client.CI.Orgs in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request >|= function
    | Error e -> Error (`Capnp e)
    | Ok x -> Ok (Results.orgs_get_list x)
end

module Org = struct
  type t = Raw.Client.Org.t Capability.t

  let repo t name =
    let open Raw.Client.Org.Repo in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.name_set params name;
    Capability.call_for_caps t method_id request Results.repo_get_pipelined

  let repos t =
    let open Raw.Client.Org.Repos in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request >|= function
    | Error e -> Error (`Capnp e)
    | Ok x -> Ok (Results.repos_get_list x)
end

module Repo = struct
  type t = Raw.Client.Repo.t Capability.t

  let refs t =
    let open Raw.Client.Repo.Refs in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request >|= function
    | Error e -> Error (`Capnp e)
    | Ok jobs ->
      let jobs =
        Results.refs_get_list jobs
        |> List.fold_left (fun acc slot ->
            let gref = Raw.Reader.RefInfo.ref_get slot in
            let hash = Raw.Reader.RefInfo.hash_get slot in
            Ref_map.add gref hash acc
          ) Ref_map.empty
      in
      Ok jobs

  let refs_of_commit t hash =
    let open Raw.Client.Repo.RefsOfCommit in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.hash_set params hash;
    Capability.call_for_value t method_id request >|= function
    | Error e -> Error (`Capnp e)
    | Ok refs -> Ok (Results.refs_get_list refs)

  let job_of_commit t hash =
    let open Raw.Client.Repo.JobOfCommit in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.hash_set params hash;
    Capability.call_for_caps t method_id request Results.job_get_pipelined

  let job_of_ref t gref =
    let open Raw.Client.Repo.JobOfRef in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.ref_set params gref;
    Capability.call_for_caps t method_id request Results.job_get_pipelined
end
