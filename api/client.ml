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
    | `Not_started -> NotStarted
    | `Pending -> Active
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
  is_experimental : bool;
}

let create_job_info ?is_experimental variant outcome ~queued_at ~started_at
    ~finished_at =
  {
    variant;
    outcome;
    queued_at;
    started_at;
    finished_at;
    is_experimental = Option.value ~default:false is_experimental;
  }

module CI = struct
  type t = Raw.Client.CI.t Capability.t
  type org_info = { name : string; number_repos : int }

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

  let orgs_detailed t =
    let open Raw.Client.CI.OrgsDetailed in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    |> Lwt_result.map @@ fun result ->
       let open Raw.Reader.OrgInfo in
       Results.orgs_get_list result
       |> List.map (fun slot ->
              let name = name_get slot in
              let number_repos = number_repos_get slot in
              { name; number_repos })
end

module Org = struct
  type t = Raw.Client.Org.t Capability.t

  type repo_info = {
    name : string;
    main_status : Build_status.t;
    main_hash : string;
    main_last_updated : float option;
    default_ref : string;
  }

  type ref_info = {
    gref : string;
    hash : string;
    status : Build_status.t;
    started_at : float option;
    ran_for : float option;
  }

  type repo_history_info = string * ref_info list

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
              let open Raw.Reader.RepoInfo in
              let name = name_get repo in
              let default_ref = default_ref_get repo in
              let main_status = main_state_get repo in
              let main_hash = main_hash_get repo in
              let main_last_updated =
                let time = main_last_updated_get repo in
                match MainLastUpdated.get time with
                | MainLastUpdated.None | MainLastUpdated.Undefined _ -> None
                | MainLastUpdated.Ts v -> Some v
              in
              { name; main_status; main_hash; main_last_updated; default_ref })

  let repo_histories t =
    let open Raw.Client.Org.RepoHistories in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    |> Lwt_result.map @@ fun histories ->
       let open Raw.Reader.RepoHistory in
       Results.histories_get_list histories
       |> List.map (fun repo_slot ->
              let repo_name = name_get repo_slot in
              let history =
                history_get_list repo_slot
                |> List.map (fun ref_slot ->
                       let open Raw.Reader.RefInfo in
                       let gref = ref_get ref_slot in
                       let hash = hash_get ref_slot in
                       let status = status_get ref_slot in
                       let started = started_at_get ref_slot in
                       let started_at =
                         match StartedAt.get started with
                         | StartedAt.None | Undefined _ -> None
                         | StartedAt.Ts v -> Some v
                       in
                       let time = ran_for_get ref_slot in
                       let ran_for =
                         match RanFor.get time with
                         | RanFor.None | RanFor.Undefined _ -> None
                         | RanFor.Ts v -> Some v
                       in
                       { gref; hash; status; started_at; ran_for })
              in
              (repo_name, history))
end

module Repo = struct
  type t = Raw.Client.Repo.t Capability.t

  type ref_info = {
    gref : string;
    hash : string;
    status : Build_status.t;
    started_at : float option;
    message : string;
    name : string;
    ran_for : float option;
  }

  let refs t =
    let open Raw.Client.Repo.Refs in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    |> Lwt_result.map @@ fun jobs ->
       Results.refs_get_list jobs
       |> List.fold_left
            (fun acc slot ->
              let open Raw.Reader.RefInfo in
              let gref = ref_get slot in
              let hash = hash_get slot in
              let status = status_get slot in
              let started_at =
                let time = started_at_get slot in
                match StartedAt.get time with
                | StartedAt.None | StartedAt.Undefined _ -> None
                | StartedAt.Ts v -> Some v
              in
              let message = message_get slot in
              let name = name_get slot in
              let ran_for =
                let time = ran_for_get slot in
                match RanFor.get time with
                | RanFor.None | RanFor.Undefined _ -> None
                | RanFor.Ts v -> Some v
              in
              let r =
                { gref; hash; status; started_at; message; name; ran_for }
              in
              Ref_map.add gref r acc)
            Ref_map.empty

  let default_ref t =
    let open Raw.Client.Repo.DefaultRef in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    |> Lwt_result.map @@ fun jobs ->
       let open Raw.Reader.RefInfo in
       let res = Results.default_get jobs in
       let gref = ref_get res in
       let hash = hash_get res in
       let status = status_get res in
       let started_at =
         let time = started_at_get res in
         match StartedAt.get time with
         | StartedAt.None | StartedAt.Undefined _ -> None
         | StartedAt.Ts v -> Some v
       in
       let message = message_get res in
       let name = name_get res in
       let ran_for =
         let time = ran_for_get res in
         match RanFor.get time with
         | RanFor.None | RanFor.Undefined _ -> None
         | RanFor.Ts v -> Some v
       in
       { gref; hash; status; started_at; message; name; ran_for }

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
       let open Raw.Reader.RefInfo in
       Results.refs_get_list history
       |> List.map (fun slot ->
              let hash = hash_get slot in
              let status = status_get slot in
              let started = started_at_get slot in
              let started_at =
                match StartedAt.get started with
                | StartedAt.None | Undefined _ -> None
                | StartedAt.Ts v -> Some v
              in
              let message = message_get slot in
              let name = name_get slot in
              let time = ran_for_get slot in
              let ran_for =
                match RanFor.get time with
                | RanFor.None | RanFor.Undefined _ -> None
                | RanFor.Ts v -> Some v
              in
              { gref; hash; status; started_at; message; name; ran_for })
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
              let open Raw.Reader.JobInfo in
              let variant = variant_get job in
              let state = state_get job in
              let outcome = State.get state in
              let queued_at = queued_at_get job in
              let queued_at =
                match QueuedAt.get queued_at with
                | QueuedAt.None | Undefined _ -> None
                | QueuedAt.Ts v -> Some v
              in
              let started_at = started_at_get job in
              let started_at =
                match StartedAt.get started_at with
                | StartedAt.None | Undefined _ -> None
                | StartedAt.Ts v -> Some v
              in
              let finished_at = finished_at_get job in
              let finished_at =
                match FinishedAt.get finished_at with
                | FinishedAt.None | Undefined _ -> None
                | FinishedAt.Ts v -> Some v
              in
              let is_experimental = is_experimental_get job in
              {
                variant;
                outcome;
                queued_at;
                started_at;
                finished_at;
                is_experimental;
              })

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

  let message t =
    let open Raw.Client.Commit.Message in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    >>= (Results.message_get >> Result.ok)
end
