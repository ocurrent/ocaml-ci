module Client = Ocaml_ci_api.Client

val to_iso8601 : float -> string

val ul_timestamps :
  queued_at:float option ->
  started_at:float option ->
  finished_at:float option ->
  [> Html_types.ul ] Tyxml_html.elt

val of_step : Client.job_info option -> [> `Div | `Ul ] Tyxml_html.elt

val show_step :
  Ocaml_ci.Run_time.Timestamp.t option ->
  build_created_at:float option ->
  [> `Div | `Ul ] Tyxml_html.elt

val show_build :
  first_step_queued_at:float option ->
  total_run_time:float ->
  [> Html_types.div ] Tyxml_html.elt

val pp_timestamp : float option -> string
val pp_duration : float option -> string
