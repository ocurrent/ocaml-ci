open Capnp_rpc_lwt

type git_ref = string
type git_hash = string
type variant = string

module Ref_map = Map.Make(String)

type job_info = {
  variant : variant;
  outcome : Raw.Reader.JobInfo.State.unnamed_union_t;
}

let pp_state f =
  let open Raw.Reader.JobInfo.State in
  function
  | NotStarted -> Fmt.string f "not started"
  | Aborted -> Fmt.string f "aborted"
  | Failed m -> Fmt.pf f "failed: %s" m
  | Passed -> Fmt.string f "passed"
  | Active -> Fmt.string f "active"
  | Undefined x -> Fmt.pf f "unknown:%d" x

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
    Capability.call_for_value t method_id request
    |> Lwt_result.map Results.orgs_get_list
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
    Capability.call_for_value t method_id request
    |> Lwt_result.map Results.repos_get_list
end

module Repo = struct
  type t = Raw.Client.Repo.t Capability.t

  let refs t =
    let open Raw.Client.Repo.Refs in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request |> Lwt_result.map @@ fun jobs ->
    Results.refs_get_list jobs
    |> List.fold_left (fun acc slot ->
        let gref = Raw.Reader.RefInfo.ref_get slot in
        let hash = Raw.Reader.RefInfo.hash_get slot in
        Ref_map.add gref hash acc
      ) Ref_map.empty

  let commit_of_hash t hash =
    let open Raw.Client.Repo.CommitOfHash in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.hash_set params hash;
    Capability.call_for_caps t method_id request Results.commit_get_pipelined

  let commit_of_ref t gref =
    let open Raw.Client.Repo.CommitOfRef in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.ref_set params gref;
    Capability.call_for_caps t method_id request Results.commit_get_pipelined
end

module Commit = struct
  type t = Raw.Client.Commit.t Capability.t

  let job_of_variant t variant =
    let open Raw.Client.Commit.JobOfVariant in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.variant_set params variant;
    Capability.call_for_caps t method_id request Results.job_get_pipelined

  let jobs t =
    let open Raw.Client.Commit.Jobs in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    |> Lwt_result.map @@ fun jobs ->
    Results.jobs_get_list jobs |> List.map (fun job ->
        let variant = Raw.Reader.JobInfo.variant_get job in
        let state = Raw.Reader.JobInfo.state_get job in
        let outcome = Raw.Reader.JobInfo.State.get state in
        { variant; outcome }
      )

  let refs t =
    let open Raw.Client.Commit.Refs in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request |> Lwt_result.map Results.refs_get_list
end
