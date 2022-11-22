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
}

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

  type repo_info = {
    name : string;
    main_status : Build_status.t;
    main_hash : string;
    main_last_updated : float option;
  }

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
              let main_status = main_state_get repo in
              let main_hash = main_hash_get repo in
              let main_last_updated =
                let time = main_last_updated_get repo in
                match MainLastUpdated.get time with
                | MainLastUpdated.None | MainLastUpdated.Undefined _ -> None
                | MainLastUpdated.Ts v -> Some v
              in
              { name; main_status; main_hash; main_last_updated })
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
       Results.refs_get_list history
       |> List.fold_left
            (fun acc slot ->
              let open Build_status in
              let open Raw.Reader.RefInfo in
              let hash = hash_get slot in
              let state = status_get slot in
              let started = started_at_get slot in
              let time =
                match StartedAt.get started with
                | StartedAt.None | Undefined _ -> None
                | StartedAt.Ts v -> Some v
              in
              match (state, Ref_map.find_opt hash acc) with
              | state, None -> Ref_map.add hash (state, time) acc
              | Passed, Some (state', _) -> Ref_map.add hash (state', time) acc
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

  let message t =
    let open Raw.Client.Commit.Message in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    >>= (Results.message_get >> Result.ok)
end
