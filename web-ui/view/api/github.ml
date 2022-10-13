module Step = Representation.Step
module Run_time = Ocaml_ci_client_lib.Run_time
module Client = Ocaml_ci_api.Client

let show_step ~step_info ~run_time =
  Dream.json
  @@ Step.to_json
  @@ Step.from_status_info_run_time ~step_info ~run_time
