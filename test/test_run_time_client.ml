module Index = Ocaml_ci.Index
module Run_time = Client_utilities.Run_time
module Job = Current.Job
module Client = Ocaml_ci_api.Client

let setup () =
  let db = Lazy.force Current.Db.v in
  Index.init ();
  Current.Db.exec_literal db "DELETE FROM cache";
  db

let database = Alcotest.(list string)
let cmp_floats v1 v2 = abs_float (v1 -. v2) < 0.0000001

let timestamps =
  let state f (st : Run_time.timestamps) =
    Fmt.pf f "%a" Run_time.pp_timestamps st
  in
  Alcotest.testable (Fmt.Dump.list state) (List.equal Run_time.eq_timestamps)

let run_time_info =
  let state f (rt : Run_time.run_time_info) =
    Fmt.pf f "%a" Run_time.pp_run_time_info rt
  in
  let equal (rt1 : Run_time.run_time_info) (rt2 : Run_time.run_time_info) =
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
  let st : Run_time.timestamps = Queued 42. in
  let expected = Run_time.Queued_for 78. in
  let result =
    Run_time.run_times_from_timestamps ~build_created_at ~current_time st
  in
  Alcotest.(check run_time_info)
    "info_from_timestamps - queued" [ expected ] [ result ]

let test_info_from_timestamps_running () =
  let build_created_at = 100. in
  let current_time = 120. in
  let st : Run_time.timestamps =
    Running { queued_at = 42.; started_at = 60. }
  in
  let expected = Run_time.Running { queued_for = 18.; ran_for = 60. } in
  let result =
    Run_time.run_times_from_timestamps ~build_created_at st ~current_time
  in
  Alcotest.(check run_time_info)
    "info_from_timestamps - running" [ expected ] [ result ]

let test_info_from_timestamps_cached () =
  let build_created_at = 100. in
  let current_time = 120. in
  let st : Run_time.timestamps =
    Finished { queued_at = 42.; started_at = Some 60.; finished_at = 82. }
  in
  let expected = Run_time.Cached (* finished < build_created_at *) in
  let result =
    Run_time.run_times_from_timestamps ~build_created_at ~current_time st
  in
  Alcotest.(check run_time_info)
    "info_from_timestamps - finished" [ expected ] [ result ]

let test_info_from_timestamps_finished () =
  let build_created_at = 100. in
  let current_time = 120. in
  let st : Run_time.timestamps =
    Finished { queued_at = 42.; started_at = Some 60.; finished_at = 182. }
  in
  let expected = Run_time.Finished { queued_for = 18.; ran_for = Some 122. } in
  let result =
    Run_time.run_times_from_timestamps ~build_created_at ~current_time st
  in
  Alcotest.(check run_time_info)
    "info_from_timestamps - finished" [ expected ] [ result ]

let test_info_from_timestamps_never_ran () =
  let build_created_at = 100. in
  let current_time = 120. in
  let st : Run_time.timestamps =
    Finished { queued_at = 42.; started_at = None; finished_at = 182. }
  in
  let expected = Run_time.Finished { queued_for = 140.; ran_for = None } in
  let result =
    Run_time.run_times_from_timestamps ~build_created_at st ~current_time
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
  let expected : Run_time.timestamps = Queued 1234567. in
  let result = Run_time.timestamps_from_job_info not_started_job in
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
  let expected : Run_time.timestamps =
    Running { queued_at = 1234567.; started_at = 1234570. }
  in
  let result = Run_time.timestamps_from_job_info active_job in
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
  let expected : Run_time.timestamps =
    Finished
      {
        queued_at = 1234567.;
        started_at = Some 1234570.;
        finished_at = 1234575.;
      }
  in
  let result = Run_time.timestamps_from_job_info finished_job in
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
    let expected : Run_time.timestamps =
      Finished
        { queued_at = 1234567.; started_at = None; finished_at = 1234575. }
    in
    let result = Run_time.timestamps_from_job_info finished_job in
    match result with
    | Error e -> Alcotest.fail e
    | Ok result ->
        Alcotest.(check timestamps)
          "timestamps_from_job_info" [ expected ] [ result ]
  in
  List.iter test [ Failed ""; Aborted ]

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
  ]
