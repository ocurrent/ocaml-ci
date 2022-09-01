module Client = Ocaml_ci_api.Client
module Run_time = Client_utilities.Run_time
open Tyxml.Html

let to_iso8601 (tt : float) =
  let ts = Timedesc.of_timestamp_float_s tt in
  Timedesc.to_iso8601 @@ Option.get ts

let ul_timestamps ~queued_at ~started_at ~finished_at =
  let queued_at = Option.fold ~none:"-" ~some:to_iso8601 queued_at in
  let started_at = Option.fold ~none:"-" ~some:to_iso8601 started_at in
  let finished_at = Option.fold ~none:"-" ~some:to_iso8601 finished_at in
  ul
    [
      li [ txt @@ Fmt.str "Queued at: %s" queued_at ];
      li [ txt @@ Fmt.str "Started at: %s" started_at ];
      li [ txt @@ Fmt.str "Finished at: %s" finished_at ];
    ]

let of_step step_info =
  match step_info with
  | None -> div [ span [ txt @@ Fmt.str "-" ] ]
  | Some step_info ->
      let queued_at = step_info.Client.queued_at in
      let started_at = step_info.Client.started_at in
      let finished_at = step_info.Client.finished_at in
      ul_timestamps ~queued_at ~started_at ~finished_at

let ul_timestamps_durations ~queued_at ~finished_at ~queued_for ~ran_for =
  let queued_for_msg, ran_for_msg =
    if queued_for = 0. && ran_for = 0. then
      ("Queued for: - (Cached)", "Ran for: - (Cached)")
    else
      ( Fmt.str "Queued for: %a" Run_time.duration_pp (Duration.of_f queued_for),
        Fmt.str "Ran for: %a" Run_time.duration_pp (Duration.of_f ran_for) )
  in
  let queued_at_msg = Option.fold ~none:"-" ~some:to_iso8601 queued_at in
  let finished_at_msg = Option.fold ~none:"-" ~some:to_iso8601 finished_at in
  ul
    [
      li
        ~a:[ a_class [ "statuses" ] ]
        [ txt @@ Fmt.str "Created at: %s" queued_at_msg ];
      li
        ~a:[ a_class [ "statuses" ] ]
        [ txt @@ Fmt.str "Finished at: %s" finished_at_msg ];
      li
        ~a:[ a_class [ "statuses" ] ]
        [ txt @@ Fmt.str "Last build %s" queued_for_msg ];
      li
        ~a:[ a_class [ "statuses" ] ]
        [ txt @@ Fmt.str "Last build %s" ran_for_msg ];
    ]

let _single_line_timestamps_durations ~queued_at ~queued_for ~ran_for =
  let queued_at = Option.fold ~none:"-" ~some:to_iso8601 queued_at in
  div
    [
      txt
      @@ Fmt.str
           "Created at: %s | Time spent in queue: %g | Time spent running: %g"
           queued_at queued_for ran_for;
    ]

let show_step (ts : Run_time.timestamps option) ~build_created_at =
  match (ts, build_created_at) with
  | None, _ | _, None -> div [ span [ txt @@ Fmt.str "-" ] ]
  | Some t, Some build_created_at -> (
      let run_times = Run_time.run_times_from_timestamps ~build_created_at t in
      let queued_for = Run_time.queued_for run_times in
      let ran_for = Run_time.ran_for run_times in
      match t with
      | Queued v ->
          ul_timestamps_durations ~queued_at:(Some v) ~finished_at:None
            ~queued_for ~ran_for
      | Running v ->
          ul_timestamps_durations ~queued_at:(Some v.queued_at)
            ~finished_at:None ~queued_for ~ran_for
      | Finished v ->
          ul_timestamps_durations ~queued_at:(Some v.queued_at)
            ~finished_at:(Some v.finished_at) ~queued_for ~ran_for)

let show_build ~first_step_queued_at ~total_run_time =
  div
    [
      span
        [
          txt
          @@ Fmt.str "Build first created at: %s"
               (to_iso8601 first_step_queued_at);
        ];
      span
        [
          txt
          @@ Fmt.str " -- Last build ran for: %a" Run_time.duration_pp
               (Duration.of_f total_run_time);
        ];
    ]
