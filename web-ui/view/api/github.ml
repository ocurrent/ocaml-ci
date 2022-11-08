module Build = Representation.Build
module Step = Representation.Step
module Run_time = Ocaml_ci_client_lib.Run_time
module Client = Ocaml_ci_api.Client

let prefix = "github"

let show_step ~step_info ~run_time ~can_rebuild =
  Dream.json
  @@ Step.to_json
  @@ Step.from_status_info_run_time ~step_info ~run_time ~can_rebuild

let list_steps ~jobs ~build_status ~step_route_prefix =
  let build_created_at =
    Run_time.build_created_at ~build:jobs
    |> Result.to_option
    |> Option.join
    |> Option.value ~default:0.
  in
  Dream.json
  @@ Build.to_json
  @@ Build.from_jobs_status ~jobs ~build_status ~build_created_at
       ~step_route_prefix
