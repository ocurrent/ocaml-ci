open Capnp_rpc_lwt

type git_ref = string
type git_hash = string
type variant = string

module Ref_map = Map.Make (String)

module State = struct
  open Raw.Reader.JobInfo.State

  type t = unnamed_union_t

  let pp f = function
    | NotStarted -> Fmt.string f "not started"
    | Aborted -> Fmt.string f "aborted"
    | Failed m -> Fmt.pf f "failed: %s" m
    | Passed -> Fmt.string f "passed"
    | Active -> Fmt.string f "active"
    | Undefined x -> Fmt.pf f "unknown:%d" x

  let from_build_status = function
    | `Failed -> Failed ""
    | `Pending | `Not_started -> NotStarted
    | `Passed -> Passed
end

module Build_status = struct
  include Raw.Reader.BuildStatus

  let pp f = function
    | NotStarted -> Fmt.string f "not started"
    | Failed -> Fmt.pf f "failed"
    | Passed -> Fmt.string f "passed"
    | Pending -> Fmt.string f "pending"
    | Undefined x -> Fmt.pf f "unknown:%d" x
end

type job_info = {
  variant : variant;
  outcome : State.t;
  queued_at : float option;
  started_at : float option;
  finished_at : float option;
}

module CI = struct
  type t = Raw.Client.CI.t Capability.t
  type org_info = { owner : string; bio : string; n_repos : int }

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
    |> Lwt_result.map
         (List.map (fun org ->
              let owner = Raw.Reader.OrgInfo.owner_get org in
              let bio = Raw.Reader.OrgInfo.bio_get org in
              let n_repos = Raw.Reader.OrgInfo.n_repos_get org in
              { owner; bio; n_repos }))
end

module Org = struct
  type t = Raw.Client.Org.t Capability.t
  type repo_info = { name : string; master_status : Build_status.t }

  let repo t name =
    let open Raw.Client.Org.Repo in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.name_set params name;
    Capability.call_for_caps t method_id request Results.repo_get_pipelined

  let repos t =
    let open Raw.Client.Org.Repos in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    |> Lwt_result.map (fun result ->
           Results.repos_get_list result
           |> List.map @@ fun repo ->
              let name = Raw.Reader.RepoInfo.name_get repo in
              let master_status = Raw.Reader.RepoInfo.master_state_get repo in
              { name; master_status })
end

module Repo = struct
  type t = Raw.Client.Repo.t Capability.t

  let refs t =
    let open Raw.Client.Repo.Refs in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    |> Lwt_result.map @@ fun jobs ->
       Results.refs_get_list jobs
       |> List.fold_left
            (fun acc slot ->
              let gref = Raw.Reader.RefInfo.ref_get slot in
              let hash = Raw.Reader.RefInfo.hash_get slot in
              let state = Raw.Reader.RefInfo.state_get slot in
              Ref_map.add gref (hash, state) acc)
            Ref_map.empty

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

  let history_of_ref t gref =
    let open Raw.Client.Repo.HistoryOfRef in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.ref_set params gref;
    Capability.call_for_value t method_id request
    |> Lwt_result.map @@ fun history ->
       Results.refs_get_list history
       |> List.fold_left
            (fun acc slot ->
              let open Build_status in
              let hash = Raw.Reader.RefInfo.hash_get slot in
              let message = Raw.Reader.RefInfo.message_get slot in
              let state = Raw.Reader.RefInfo.state_get slot in
              let started = Raw.Reader.RefInfo.started_get slot in
              let time =
                match Raw.Reader.RefInfo.Started.get started with
                | Raw.Reader.RefInfo.Started.None | Undefined _ -> None
                | Raw.Reader.RefInfo.Started.Ts v -> Some v
              in
              match (state, Ref_map.find_opt hash acc) with
              | state, None -> Ref_map.add hash (message, state, time) acc
              | Passed, Some (message', state', _) ->
                  Ref_map.add hash (message', state', time) acc
              | Failed, _ | _ -> acc)
            Ref_map.empty
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
       Results.jobs_get_list jobs
       |> List.map (fun job ->
              let variant = Raw.Reader.JobInfo.variant_get job in
              let state = Raw.Reader.JobInfo.state_get job in
              let outcome = Raw.Reader.JobInfo.State.get state in
              let queued_at = Raw.Reader.JobInfo.queued_at_get job in
              let queued_at =
                match Raw.Reader.JobInfo.QueuedAt.get queued_at with
                | Raw.Reader.JobInfo.QueuedAt.None | Undefined _ -> None
                | Raw.Reader.JobInfo.QueuedAt.Ts v -> Some v
              in
              let started_at = Raw.Reader.JobInfo.started_at_get job in
              let started_at =
                match Raw.Reader.JobInfo.StartedAt.get started_at with
                | Raw.Reader.JobInfo.StartedAt.None | Undefined _ -> None
                | Raw.Reader.JobInfo.StartedAt.Ts v -> Some v
              in
              let finished_at = Raw.Reader.JobInfo.finished_at_get job in
              let finished_at =
                match Raw.Reader.JobInfo.FinishedAt.get finished_at with
                | Raw.Reader.JobInfo.FinishedAt.None | Undefined _ -> None
                | Raw.Reader.JobInfo.FinishedAt.Ts v -> Some v
              in
              { variant; outcome; queued_at; started_at; finished_at })

  let refs t =
    let open Raw.Client.Commit.Refs in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    |> Lwt_result.map Results.refs_get_list

  let ( >> ) f g x = g (f x)
  let ( >>= ) = Lwt_result.bind_result

  let status t =
    let open Raw.Client.Commit.Status in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    >>= (Results.status_get >> function
         | NotStarted -> Ok `Not_started
         | Passed -> Ok `Passed
         | Failed -> Ok `Failed
         | Pending -> Ok `Pending
         | Undefined i ->
             Error (`Msg (Fmt.str "client.states: undefined state %d" i)))
end
