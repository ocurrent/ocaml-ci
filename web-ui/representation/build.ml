module Run_time = Ocaml_ci.Run_time
module Client = Ocaml_ci_api.Client

let version = "1.0"

type t = {
  version : string;
  status : string;
  first_created_at : string;
  ran_for : string;
  total_ran_for : string;
  can_cancel : bool;
  can_rebuild : bool;
  steps : Step.t list;
  step_route_prefix : string;
}
[@@deriving yojson]

let to_json t = Yojson.Safe.to_string @@ to_yojson t

let from_jobs_status ~jobs ~build_status ~build_created_at ~step_route_prefix =
  let first_step_queued_at =
    match Run_time.Job.first_step_queued_at jobs with
    | Error e ->
        Dream.log "Error - %s" e;
        None
    | Ok v -> Some v
  in
  let total_run_time = Run_time.Job.total_of_run_times jobs in
  let build_run_time = Run_time.Job.build_run_time jobs in
  let can_cancel =
    let check job_info =
      match job_info.Client.outcome with
      | Active | NotStarted -> true
      | Aborted | Failed _ | Passed | Undefined _ -> false
    in
    List.exists check jobs
  in
  let can_rebuild =
    if can_cancel then false
    else
      let check job_info =
        match job_info.Client.outcome with
        | Active | NotStarted | Passed -> false
        | Aborted | Failed _ | Undefined _ -> true
      in
      List.exists check jobs
  in
  let steps =
    List.map
      (fun j -> Step.from_status_info ~step_info:(Some j) ~build_created_at)
      jobs
  in
  {
    version;
    status = Fmt.str "%a" Client.State.pp build_status;
    first_created_at = Run_time.Duration.pp_readable_opt first_step_queued_at;
    total_ran_for = Run_time.Duration.pp_opt @@ Some total_run_time;
    ran_for = Run_time.Duration.pp_opt @@ Some build_run_time;
    can_cancel;
    can_rebuild;
    steps;
    step_route_prefix;
  }
