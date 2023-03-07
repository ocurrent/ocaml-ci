module Index = Ocaml_ci.Index
module Run_time = Ocaml_ci.Run_time
module Job = Current.Job
module Client = Ocaml_ci_api.Client

let database = Alcotest.(list string)
let cmp_floats v1 v2 = abs_float (v1 -. v2) < 0.0000001

let timestamps =
  let state f (st : Run_time.Timestamp.t) =
    Fmt.pf f "%a" Run_time.Timestamp.pp st
  in
  Alcotest.testable (Fmt.Dump.list state) (List.equal Run_time.Timestamp.eq)

let run_time_info =
  let state f (rt : Run_time.TimeInfo.t) =
    Fmt.pf f "%a" Run_time.TimeInfo.pp rt
  in
  let equal (rt1 : Run_time.TimeInfo.t) (rt2 : Run_time.TimeInfo.t) =
    match (rt1, rt2) with
    | Cached, Cached -> true
    | Queued_for v1, Queued_for v2 -> cmp_floats v1 v2
    | Running v1, Running v2 ->
        cmp_floats v1.queued_for v2.queued_for
        && cmp_floats v1.ran_for v2.ran_for
    | Finished v1, Finished v2 -> (
        match (v1.ran_for, v2.ran_for) with
        | None, None -> cmp_floats v1.queued_for v2.queued_for
        | Some r1, Some r2 ->
            cmp_floats v1.queued_for v2.queued_for && cmp_floats r1 r2
        | None, Some _ | Some _, None -> false)
    | Cached, (Queued_for _ | Running _ | Finished _) -> false
    | Queued_for _, (Cached | Running _ | Finished _) -> false
    | Running _, (Cached | Queued_for _ | Finished _) -> false
    | Finished _, (Cached | Queued_for _ | Running _) -> false
  in
  Alcotest.testable (Fmt.Dump.list state) (List.equal equal)

let test_info_from_timestamps_queued () =
  let build_created_at = 100. in
  let current_time = 120. in
  let st : Run_time.Timestamp.t = Queued 42. in
  let expected = Run_time.TimeInfo.Queued_for 78. in
  let result =
    Run_time.TimeInfo.of_timestamp ~build_created_at ~current_time st
  in
  Alcotest.(check run_time_info)
    "info_from_timestamps - queued" [ expected ] [ result ]

let test_info_from_timestamps_running () =
  let build_created_at = 100. in
  let current_time = 120. in
  let st : Run_time.Timestamp.t =
    Running { queued_at = 42.; started_at = 60. }
  in
  let expected =
    Run_time.TimeInfo.Running { queued_for = 18.; ran_for = 60. }
  in
  let result =
    Run_time.TimeInfo.of_timestamp ~build_created_at st ~current_time
  in
  Alcotest.(check run_time_info)
    "info_from_timestamps - running" [ expected ] [ result ]

let test_info_from_timestamps_cached () =
  let build_created_at = 100. in
  let current_time = 120. in
  let st : Run_time.Timestamp.t =
    Finished { queued_at = 42.; started_at = Some 60.; finished_at = 82. }
  in
  let expected =
    Run_time.TimeInfo.Cached (* because finished < build_created_at *)
  in
  let result =
    Run_time.TimeInfo.of_timestamp ~build_created_at ~current_time st
  in
  Alcotest.(check run_time_info)
    "info_from_timestamps - finished" [ expected ] [ result ]

let test_info_from_timestamps_finished () =
  let build_created_at = 100. in
  let current_time = 120. in
  let st : Run_time.Timestamp.t =
    Finished { queued_at = 42.; started_at = Some 60.; finished_at = 182. }
  in
  let expected =
    Run_time.TimeInfo.Finished { queued_for = 18.; ran_for = Some 122. }
  in
  let result =
    Run_time.TimeInfo.of_timestamp ~build_created_at ~current_time st
  in
  Alcotest.(check run_time_info)
    "info_from_timestamps - finished" [ expected ] [ result ]

let test_info_from_timestamps_never_ran () =
  let build_created_at = 100. in
  let current_time = 120. in
  let st : Run_time.Timestamp.t =
    Finished { queued_at = 42.; started_at = None; finished_at = 182. }
  in
  let expected =
    Run_time.TimeInfo.Finished { queued_for = 140.; ran_for = None }
  in
  let result =
    Run_time.TimeInfo.of_timestamp ~build_created_at st ~current_time
  in
  Alcotest.(check run_time_info)
    "info_from_timestamps - finished" [ expected ] [ result ]

let test_timestamps_from_job_info_queued =
  let not_started_job : Client.job_info =
    {
      variant = "variant";
      outcome = NotStarted;
      queued_at = Some 1234567.;
      started_at = None;
      finished_at = None;
    }
  in
  let expected : Run_time.Timestamp.t = Queued 1234567. in
  let result = Run_time.Timestamp.of_job_info not_started_job in
  match result with
  | Error e -> Alcotest.fail e
  | Ok result ->
      Alcotest.(check timestamps)
        "timestamps_from_job_info" [ expected ] [ result ]

let test_timestamps_from_job_info_running =
  let active_job : Client.job_info =
    {
      variant = "variant";
      outcome = Active;
      queued_at = Some 1234567.;
      started_at = Some 1234570.;
      finished_at = None;
    }
  in
  let expected : Run_time.Timestamp.t =
    Running { queued_at = 1234567.; started_at = 1234570. }
  in
  let result = Run_time.Timestamp.of_job_info active_job in
  match result with
  | Error e -> Alcotest.fail e
  | Ok result ->
      Alcotest.(check timestamps)
        "timestamps_from_job_info" [ expected ] [ result ]

let test_timestamps_from_job_info_finished =
  let finished_job : Client.job_info =
    {
      variant = "variant";
      outcome = Passed;
      queued_at = Some 1234567.;
      started_at = Some 1234570.;
      finished_at = Some 1234575.;
    }
  in
  let expected : Run_time.Timestamp.t =
    Finished
      {
        queued_at = 1234567.;
        started_at = Some 1234570.;
        finished_at = 1234575.;
      }
  in
  let result = Run_time.Timestamp.of_job_info finished_job in
  match result with
  | Error e -> Alcotest.fail e
  | Ok result ->
      Alcotest.(check timestamps)
        "timestamps_from_job_info" [ expected ] [ result ]

let test_timestamps_from_job_info_finished_with_errors =
  let test (outcome : Client.State.t) =
    let finished_job : Client.job_info =
      {
        variant = "variant";
        outcome;
        queued_at = Some 1234567.;
        started_at = None;
        finished_at = Some 1234575.;
      }
    in
    let expected : Run_time.Timestamp.t =
      Finished
        { queued_at = 1234567.; started_at = None; finished_at = 1234575. }
    in
    let result = Run_time.Timestamp.of_job_info finished_job in
    match result with
    | Error e -> Alcotest.fail e
    | Ok result ->
        Alcotest.(check timestamps)
          "timestamps_from_job_info" [ expected ] [ result ]
  in
  List.iter test [ Failed ""; Aborted ]

let test_queued_for_cached =
  let rt : Run_time.TimeInfo.t = Cached in
  let expected = 0. in
  let result = Run_time.TimeInfo.queued_for rt in
  Alcotest.(check (float 0.001)) "queued_for Cached" expected result

let test_queued_for_queued =
  let rt : Run_time.TimeInfo.t = Queued_for 42.2 in
  let expected = 42.2 in
  let result = Run_time.TimeInfo.queued_for rt in
  Alcotest.(check (float 0.001)) "queued_for Queued" expected result

let test_queued_for_running =
  let rt : Run_time.TimeInfo.t =
    Running { queued_for = 42.2; ran_for = 24.4 }
  in
  let expected = 42.2 in
  let result = Run_time.TimeInfo.queued_for rt in
  Alcotest.(check (float 0.001)) "queued_for Running" expected result

let test_queued_for_finished =
  let rt : Run_time.TimeInfo.t =
    Finished { queued_for = 42.2; ran_for = Some 24.4 }
  in
  let rt' : Run_time.TimeInfo.t =
    Finished { queued_for = 42.2; ran_for = None }
  in
  let expected = 42.2 in
  let result = Run_time.TimeInfo.queued_for rt in
  let result' = Run_time.TimeInfo.queued_for rt' in
  Alcotest.(check (float 0.001)) "queued_for Finished" expected result;
  Alcotest.(check (float 0.001)) "queued_for Finished" expected result'

let test_ran_for_cached =
  let rt : Run_time.TimeInfo.t = Cached in
  let expected = 0. in
  let result = Run_time.TimeInfo.queued_for rt in
  Alcotest.(check (float 0.001)) "ran_for Cached" expected result

let test_ran_for_queued =
  let rt : Run_time.TimeInfo.t = Queued_for 42.2 in
  let expected = 0. in
  let result = Run_time.TimeInfo.ran_for rt in
  Alcotest.(check (float 0.001)) "ran_for Queued" expected result

let test_ran_for_running =
  let rt : Run_time.TimeInfo.t =
    Running { queued_for = 42.2; ran_for = 24.4 }
  in
  let expected = 24.4 in
  let result = Run_time.TimeInfo.ran_for rt in
  Alcotest.(check (float 0.001)) "ran_for Running" expected result

let test_ran_for_finished =
  let rt : Run_time.TimeInfo.t =
    Finished { queued_for = 42.2; ran_for = Some 24.4 }
  in
  let rt' : Run_time.TimeInfo.t =
    Finished { queued_for = 42.2; ran_for = None }
  in
  let expected = 24.4 in
  let result = Run_time.TimeInfo.ran_for rt in
  Alcotest.(check (float 0.001)) "ran_for Finished" expected result;
  let expected' = 0. in
  let result' = Run_time.TimeInfo.ran_for rt' in
  Alcotest.(check (float 0.001)) "ran_for Finished" expected' result'

let test_total_time_cached =
  let rt : Run_time.TimeInfo.t = Cached in
  let expected = 0. in
  let result = Run_time.TimeInfo.queued_for rt in
  Alcotest.(check (float 0.001)) "total_time Cached" expected result

let test_total_time_queued =
  let rt : Run_time.TimeInfo.t = Queued_for 42.2 in
  let expected = 42.2 in
  let result = Run_time.TimeInfo.total rt in
  Alcotest.(check (float 0.001)) "total_time Queued" expected result

let test_total_time_running =
  let rt : Run_time.TimeInfo.t =
    Running { queued_for = 42.2; ran_for = 24.4 }
  in
  let expected = 66.6 in
  let result = Run_time.TimeInfo.total rt in
  Alcotest.(check (float 0.001)) "ran_for Running" expected result

let test_total_time_finished =
  let rt : Run_time.TimeInfo.t =
    Finished { queued_for = 42.2; ran_for = Some 24.4 }
  in
  let rt' : Run_time.TimeInfo.t =
    Finished { queued_for = 42.2; ran_for = None }
  in
  let expected = 66.6 in
  let result = Run_time.TimeInfo.total rt in
  Alcotest.(check (float 0.001)) "ran_for Finished" expected result;
  let expected' = 42.2 in
  let result' = Run_time.TimeInfo.total rt' in
  Alcotest.(check (float 0.001)) "ran_for Finished" expected' result'

let test_build_created_at_empty =
  let expected = Error "No analysis step found" in
  let result = Run_time.Job.build_created_at ~build:[] in
  Alcotest.(check (Alcotest.result (option (float 0.001)) string))
    "build_created_for empty" expected result

let test_build_created_at_happy_path =
  let analysis_step : Client.job_info =
    {
      variant = "(analysis)";
      outcome = Passed;
      queued_at = Some 1234567.;
      started_at = Some 1234570.;
      finished_at = Some 1234575.;
    }
  in
  let lint_step : Client.job_info =
    {
      variant = "(lint-fmt)";
      outcome = Passed;
      queued_at = Some 1234576.;
      started_at = Some 1234579.;
      finished_at = Some 1234585.;
    }
  in
  let build_step : Client.job_info =
    {
      variant = "foo-11-4.14";
      outcome = Active;
      queued_at = Some 1234576.;
      started_at = Some 1234579.;
      finished_at = None;
    }
  in
  let build = [ analysis_step; lint_step; build_step ] in
  let expected = Ok (Some 1234567.) in
  let result = Run_time.Job.build_created_at ~build in
  Alcotest.(check (Alcotest.result (option (float 0.001)) string))
    "build_created_for happy" expected result

let test_build_created_at_mangled =
  let analysis_step : Client.job_info =
    {
      variant = "(analysis)";
      outcome = Passed;
      queued_at = Some 1234567.;
      started_at = Some 1234570.;
      finished_at = Some 1234575.;
    }
  in
  let analysis_step_2 : Client.job_info =
    {
      variant = "(analysis)";
      outcome = Passed;
      queued_at = Some 1234576.;
      started_at = Some 1234579.;
      finished_at = Some 1234585.;
    }
  in
  let build_step : Client.job_info =
    {
      variant = "foo-11-4.14";
      outcome = Active;
      queued_at = Some 1234576.;
      started_at = Some 1234579.;
      finished_at = None;
    }
  in
  let build = [ analysis_step; analysis_step_2; build_step ] in
  let expected = Error "Multiple analysis steps found" in
  let result = Run_time.Job.build_created_at ~build in
  Alcotest.(check (Alcotest.result (option (float 0.001)) string))
    "build_created_for multiple" expected result

let test_total_of_run_times =
  let analysis_step : Client.job_info =
    {
      variant = "(analysis)";
      outcome = Passed;
      queued_at = Some 1234567.;
      started_at = Some 1234570.;
      finished_at = Some 1234575.;
    }
  in
  let lint_step : Client.job_info =
    {
      variant = "(lint-fmt)";
      outcome = Passed;
      queued_at = Some 1234576.;
      started_at = Some 1234579.;
      finished_at = Some 1234585.;
    }
  in
  let build_step : Client.job_info =
    {
      variant = "foo-11-4.14";
      outcome = Passed;
      queued_at = Some 1234576.;
      started_at = Some 1234579.;
      finished_at = Some 1234590.;
    }
  in
  let build = [ analysis_step; lint_step; build_step ] in
  let expected = 3. +. 5. +. 3. +. 6. +. 3. +. 11. in
  let result = Run_time.Job.total_of_run_times build in
  Alcotest.(check (float 0.001)) "total_of_run_times" expected result

let test_build_run_time =
  let analysis_step : Client.job_info =
    {
      variant = "(analysis)";
      outcome = Passed;
      queued_at = Some 1234567.;
      started_at = Some 1234570.;
      finished_at = Some 1234575.;
    }
    (* thus run-time of analysis step is 3 + 5 = 8 *)
  in
  let lint_step : Client.job_info =
    {
      variant = "(lint-fmt)";
      outcome = Passed;
      queued_at = Some 1234576.;
      started_at = Some 1234579.;
      finished_at = Some 1234585.;
    }
    (* run-time of lint_step is 3 + 6 = 9 *)
  in
  let build_step : Client.job_info =
    {
      variant = "foo-11-4.14";
      outcome = Passed;
      queued_at = Some 1234576.;
      started_at = Some 1234579.;
      finished_at = Some 1234590.;
    }
    (* run-time of build_step is 3 + 11 = 14 -- this is the longest running step *)
  in
  let build = [ analysis_step; lint_step; build_step ] in
  let expected = 8. +. 14. in
  (* run-time of analysis + run-time of build-step (since it is the longest running step)*)
  let result = Run_time.Job.build_run_time build in
  Alcotest.(check (float 0.001)) "build_run_time" expected result

let test_first_step_queued_at =
  let not_started : Client.job_info =
    {
      variant = "variant";
      outcome = NotStarted;
      queued_at = None;
      started_at = None;
      finished_at = None;
    }
  in
  let step_1 : Client.job_info =
    {
      variant = "variant";
      outcome = Passed;
      queued_at = Some 1234567.;
      started_at = Some 1234570.;
      finished_at = Some 1234575.;
    }
  in
  let step_2 : Client.job_info =
    {
      variant = "variant";
      outcome = Passed;
      queued_at = Some 1234576.;
      started_at = Some 1234579.;
      finished_at = Some 1234585.;
    }
  in
  let step_3 : Client.job_info =
    {
      variant = "variant-11-4.14";
      outcome = Passed;
      queued_at = Some 1234576.;
      started_at = Some 1234579.;
      finished_at = Some 1234590.;
    }
  in
  let empty = [] in
  let expected = Error "Empty build" in
  let result = Run_time.Job.first_step_queued_at empty in
  Alcotest.(check (result (float 0.001) string))
    "first_step_queued_at Empty" expected result;

  let build = [ step_1; step_2; step_3 ] in
  let expected = Ok 1234567. in
  let result = Run_time.Job.first_step_queued_at build in
  Alcotest.(check (result (float 0.001) string))
    "first_step_queued_at not-empty" expected result;

  let build = [ not_started ] in
  let expected = Error "Empty build" in
  let result = Run_time.Job.first_step_queued_at build in
  Alcotest.(check (result (float 0.001) string))
    "first_step_queued_at no-timestamp" expected result

let test_duration_pp =
  let ten_power_9 : int64 = 1000000000L in

  let expected = "0s" in
  let result = Fmt.str "%a" Run_time.Duration.pp 0L in
  Alcotest.(check string) "Os are printed without microseconds" expected result;

  let expected = "12s" in
  let result = Fmt.str "%a" Run_time.Duration.pp (Int64.mul 12L ten_power_9) in
  Alcotest.(check string) "Os are printed without microseconds" expected result;

  let expected = "1m02s" in
  let result = Fmt.str "%a" Run_time.Duration.pp (Int64.mul 62L ten_power_9) in
  Alcotest.(check string) "Minutes and seconds" expected result;

  let expected = "1h01m" in
  let result =
    Fmt.str "%a" Run_time.Duration.pp (Int64.mul 3662L ten_power_9)
  in
  Alcotest.(check string)
    "Seconds are not printed for durations in excess of an hour" expected result;

  let expected = "1d00h" in
  let result =
    Fmt.str "%a" Run_time.Duration.pp (Int64.mul 86462L ten_power_9)
  in
  Alcotest.(check string)
    "Minutes are not printed for durations in excess of a day" expected result

let tests =
  [
    Alcotest_lwt.test_case_sync "info_from_timestamps_queued" `Quick
      test_info_from_timestamps_queued;
    Alcotest_lwt.test_case_sync "info_from_timestamps_running" `Quick
      test_info_from_timestamps_running;
    Alcotest_lwt.test_case_sync "info_from_timestamps_finished" `Quick
      test_info_from_timestamps_finished;
    Alcotest_lwt.test_case_sync "info_from_timestamps_cached" `Quick
      test_info_from_timestamps_cached;
    Alcotest_lwt.test_case_sync "info_from_timestamps_never_ran" `Quick
      test_info_from_timestamps_never_ran;
    Alcotest_lwt.test_case_sync "timestamps_from_job_info_queued" `Quick
      (fun () -> test_timestamps_from_job_info_queued);
    Alcotest_lwt.test_case_sync "timestamps_from_job_info_running" `Quick
      (fun () -> test_timestamps_from_job_info_running);
    Alcotest_lwt.test_case_sync "timestamps_from_job_info_finished" `Quick
      (fun () -> test_timestamps_from_job_info_finished);
    Alcotest_lwt.test_case_sync "timestamps_from_job_info_finished_error" `Quick
      (fun () -> test_timestamps_from_job_info_finished_with_errors);
    Alcotest_lwt.test_case_sync "queued_for_cached" `Quick (fun () ->
        test_queued_for_cached);
    Alcotest_lwt.test_case_sync "queued_for_queued" `Quick (fun () ->
        test_queued_for_queued);
    Alcotest_lwt.test_case_sync "queued_for_running" `Quick (fun () ->
        test_queued_for_running);
    Alcotest_lwt.test_case_sync "queued_for_finished" `Quick (fun () ->
        test_queued_for_finished);
    Alcotest_lwt.test_case_sync "ran_for_cached" `Quick (fun () ->
        test_ran_for_cached);
    Alcotest_lwt.test_case_sync "ran_for_queued" `Quick (fun () ->
        test_ran_for_queued);
    Alcotest_lwt.test_case_sync "ran_for_running" `Quick (fun () ->
        test_ran_for_running);
    Alcotest_lwt.test_case_sync "ran_for_finished" `Quick (fun () ->
        test_ran_for_finished);
    Alcotest_lwt.test_case_sync "total_time_cached" `Quick (fun () ->
        test_total_time_cached);
    Alcotest_lwt.test_case_sync "total_time_queued" `Quick (fun () ->
        test_total_time_queued);
    Alcotest_lwt.test_case_sync "total_time_running" `Quick (fun () ->
        test_total_time_running);
    Alcotest_lwt.test_case_sync "total_time_finished" `Quick (fun () ->
        test_total_time_finished);
    Alcotest_lwt.test_case_sync "build_created_at Empty" `Quick (fun () ->
        test_build_created_at_empty);
    Alcotest_lwt.test_case_sync "build_created_at Happy Path" `Quick (fun () ->
        test_build_created_at_happy_path);
    Alcotest_lwt.test_case_sync "build_created_at Mangled" `Quick (fun () ->
        test_build_created_at_mangled);
    Alcotest_lwt.test_case_sync "total of run times" `Quick (fun () ->
        test_total_of_run_times);
    Alcotest_lwt.test_case_sync "first step queued at" `Quick (fun () ->
        test_first_step_queued_at);
    Alcotest_lwt.test_case_sync "duration_pp" `Quick (fun () ->
        test_duration_pp);
  ]
