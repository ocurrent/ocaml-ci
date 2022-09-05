module Client = Ocaml_ci_api.Client
module Run_time = Ocaml_ci_client_lib.Run_time

val to_iso8601 : float -> string

val ul_timestamps :
  queued_at:float option ->
  started_at:float option ->
  finished_at:float option ->
  [> Html_types.ul ] Tyxml_html.elt

val of_step : Client.job_info option -> [> `Div | `Ul ] Tyxml_html.elt

val show_step :
  Run_time.timestamps option ->
  build_created_at:float option ->
  [> `Div | `Ul ] Tyxml_html.elt

val show_build :
  first_step_queued_at:float option ->
  total_run_time:float ->
  [> Html_types.div ] Tyxml_html.elt
