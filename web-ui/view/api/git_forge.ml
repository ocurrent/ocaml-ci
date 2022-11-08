module type Api = sig
  module Client = Ocaml_ci_api.Client
  module Run_time = Ocaml_ci_client_lib.Run_time

  val prefix : string

  val show_step :
    step_info:Client.job_info option ->
    run_time:Run_time.run_time_info option ->
    can_rebuild:bool ->
    Dream.response Lwt.t

  val list_steps :
    jobs:Client.job_info list ->
    build_status:Client.State.t ->
    step_route_prefix:string ->
    Dream.response Lwt.t
end