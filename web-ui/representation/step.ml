module Timestamps_durations = View.Timestamps_durations
module Run_time = Ocaml_ci_client_lib.Run_time
module Client = Ocaml_ci_api.Client

let version = "1.0"

type t = {
  version : string;
  status : string;
  created_at : string;
  finished_at : string;
  queued_for : string;
  ran_for : string;
}
[@@deriving yojson]

let from_status_info_run_time ~step_info ~run_time =
  {
    version;
    status =
      Option.fold ~none:""
        ~some:(fun i -> Fmt.str "%a" Client.State.pp i.Client.outcome)
        step_info;
    created_at =
      Option.fold ~none:""
        ~some:(fun i -> Timestamps_durations.pp_timestamp i.Client.queued_at)
        step_info;
    finished_at =
      Option.fold ~none:""
        ~some:(fun i -> Timestamps_durations.pp_timestamp i.Client.finished_at)
        step_info;
    queued_for =
      Timestamps_durations.pp_duration (Option.map Run_time.queued_for run_time);
    ran_for =
      Timestamps_durations.pp_duration (Option.map Run_time.ran_for run_time);
  }

let to_json t = Yojson.Safe.to_string @@ to_yojson t
