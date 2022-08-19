module Index = Ocaml_ci.Index
module Run_time = Ocaml_ci.Run_time
module Job = Current.Job

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
  let equal (st1 : Run_time.timestamps) (st2 : Run_time.timestamps) =
    match (st1, st2) with
    | Queued v1, Queued v2 -> cmp_floats v1 v2
    | Running v1, Running v2 ->
        cmp_floats v1.ready v2.ready && cmp_floats v1.started v2.started
    | ( Finished { ready = ready1; started = None; finished = finished1 },
        Finished { ready = ready2; started = None; finished = finished2 } ) ->
        cmp_floats ready1 ready2 && cmp_floats finished1 finished2
    | ( Finished
          { ready = ready1; started = Some started1; finished = finished1 },
        Finished
          { ready = ready2; started = Some started2; finished = finished2 } ) ->
        cmp_floats ready1 ready2
        && cmp_floats started1 started2
        && cmp_floats finished1 finished2
    | Queued _, (Running _ | Finished _) -> false
    | Running _, (Queued _ | Finished _) -> false
    | ( Finished { started = None; _ },
        (Queued _ | Running _ | Finished { started = Some _; _ }) ) ->
        false
    | ( Finished { started = Some _; _ },
        (Queued _ | Running _ | Finished { started = None; _ }) ) ->
        false
  in

  Alcotest.testable (Fmt.Dump.list state) (List.equal equal)

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

let test_running () =
  (Job.timestamp := fun () -> 0.1);
  (* Note that this is what is used for the starting-at timestamp *)
  let switch = Current.Switch.create ~label:"output" () in
  let config = Current.Config.v () in
  let pool = Current.Pool.create ~label:"test" 1 in
  let job1 = Job.create ~switch ~label:"output" ~config () in
  let _ = Job.start ~pool ~level:Current.Level.Harmless job1 in
  let db = setup () in
  Alcotest.check database "Disk store initially empty" [] @@ [];
  Current.Db.exec_literal db
    (Fmt.str
       "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, \
        running, finished, build) \n\
        VALUES ('test42', x'00', '%s', x'01', 1, x'02', '1970-01-01 00:00', \
        '1970-01-01 00:01', '1970-01-01 00:00', 0)"
       (Job.id job1));
  let expected : Run_time.timestamps =
    Run_time.Running { ready = 0.; started = 0.1 }
  in
  let result = Option.get (Run_time.timestamps_of_job @@ Job.id job1) in
  Alcotest.(check timestamps) "Running" [ expected ] [ result ]

let test_simple_run_times () =
  let db = setup () in
  Alcotest.check database "Disk store initially empty" [] @@ [];
  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build) \n\
     VALUES ('test42', x'00', 'job42', x'01', 1, x'02', '2019-11-01 09:00', \
     '2019-11-01 09:05', '2019-11-01 10:04', 0)";
  (* 2019-11-01 09:00:00 is 1572598800 milliseconds from epoch *)
  let expected : Run_time.timestamps =
    Finished
      {
        ready = 1572598800.;
        started = Some 1572599100.;
        finished = 1572602640.;
      }
  in
  let result = Option.get (Run_time.timestamps_of_job "job42") in
  Alcotest.(check timestamps) "Finished" [ expected ] [ result ];

  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, finished, \
     build) \n\
     VALUES ('test43', x'00', 'job43', x'01', 1, x'02', '2019-11-01 09:00', \
     '2019-11-01 10:04', 0)";
  let expected = Run_time.Queued 1572598800. in
  let result = Option.get (Run_time.timestamps_of_job "job43") in
  Alcotest.(check timestamps) "Queued" [ expected ] [ result ]

let test_info_from_timestamps_queued () =
  let current_time = 100. in
  let st : Run_time.timestamps = Queued 42. in
  let expected = Run_time.Queued_for 58. in
  let result = Run_time.info_from_timestamps ~current_time st in
  Alcotest.(check run_time_info)
    "info_from_timestamps - queued" [ expected ] [ result ]

let test_info_from_timestamps_running () =
  let current_time = 100. in
  let st : Run_time.timestamps = Running { ready = 42.; started = 60. } in
  let expected = Run_time.Running { queued_for = 18.; ran_for = 40. } in
  let result = Run_time.info_from_timestamps ~current_time st in
  Alcotest.(check run_time_info)
    "info_from_timestamps - running" [ expected ] [ result ]

let test_info_from_timestamps_cached () =
  let current_time = 100. in
  let st : Run_time.timestamps =
    Finished { ready = 42.; started = Some 60.; finished = 82. }
  in
  let expected = Run_time.Cached (* finished < current_time *) in
  let result = Run_time.info_from_timestamps ~current_time st in
  Alcotest.(check run_time_info)
    "info_from_timestamps - finished" [ expected ] [ result ]

let test_info_from_timestamps_finished () =
  let current_time = 100. in
  let st : Run_time.timestamps =
    Finished { ready = 42.; started = Some 60.; finished = 182. }
  in
  let expected = Run_time.Finished { queued_for = 18.; ran_for = Some 122. } in
  let result = Run_time.info_from_timestamps ~current_time st in
  Alcotest.(check run_time_info)
    "info_from_timestamps - finished" [ expected ] [ result ]

let test_info_from_timestamps_never_ran () =
  let current_time = 100. in
  let st : Run_time.timestamps =
    Finished { ready = 42.; started = None; finished = 182. }
  in
  let expected = Run_time.Finished { queued_for = 140.; ran_for = None } in
  let result = Run_time.info_from_timestamps ~current_time st in
  Alcotest.(check run_time_info)
    "info_from_timestamps - finished" [ expected ] [ result ]

let test_merge_queued_queued () =
  let st1 : Run_time.timestamps = Queued 42. in
  let st2 : Run_time.timestamps = Queued 22. in
  let expected : Run_time.timestamps = Queued 22. in
  let result = Run_time.merge st1 st2 in
  Alcotest.(check timestamps) "merge_queued_queued" [ expected ] [ result ]

let test_merge_queued_running () =
  let st1 : Run_time.timestamps = Queued 12. in
  let st2 : Run_time.timestamps = Running { ready = 22.; started = 40. } in
  let expected : Run_time.timestamps = Running { ready = 12.; started = 40. } in
  let result = Run_time.merge st1 st2 in
  Alcotest.(check timestamps) "merge_queued_running" [ expected ] [ result ]

let test_merge_queued_finished () =
  let st1 : Run_time.timestamps = Queued 12. in
  let st2 : Run_time.timestamps =
    Finished { ready = 22.; started = Some 40.; finished = 55. }
  in
  let expected : Run_time.timestamps = Queued 12. in
  let result = Run_time.merge st1 st2 in
  Alcotest.(check timestamps) "merge_queued_finished" [ expected ] [ result ]

let test_merge_running_running () =
  let st1 : Run_time.timestamps = Running { ready = 12.; started = 42. } in
  let st2 : Run_time.timestamps = Running { ready = 22.; started = 30. } in
  let expected : Run_time.timestamps = Running { ready = 12.; started = 30. } in
  let result = Run_time.merge st1 st2 in
  Alcotest.(check timestamps) "merge_running_running" [ expected ] [ result ]

let test_merge_running_finished () =
  let st1 : Run_time.timestamps = Running { ready = 12.; started = 42. } in
  let st2 : Run_time.timestamps =
    Finished { ready = 22.; started = Some 40.; finished = 55. }
  in
  let expected : Run_time.timestamps = Running { ready = 12.; started = 40. } in
  let result = Run_time.merge st1 st2 in
  Alcotest.(check timestamps) "merge_running_finished" [ expected ] [ result ];

  let st2 : Run_time.timestamps =
    Finished { ready = 22.; started = None; finished = 55. }
  in
  let expected : Run_time.timestamps = Running { ready = 12.; started = 42. } in
  let result = Run_time.merge st1 st2 in
  Alcotest.(check timestamps) "merge_running_finished" [ expected ] [ result ]

let test_merge_finished_finished () =
  let st1 : Run_time.timestamps =
    Finished { ready = 12.; started = Some 40.; finished = 55. }
  in
  let st2 : Run_time.timestamps =
    Finished { ready = 22.; started = Some 30.; finished = 45. }
  in
  let expected : Run_time.timestamps =
    Finished { ready = 12.; started = Some 30.; finished = 55. }
  in
  let result = Run_time.merge st1 st2 in
  Alcotest.(check timestamps) "merge_finished_finished" [ expected ] [ result ];

  let st2 : Run_time.timestamps =
    Finished { ready = 22.; started = None; finished = 45. }
  in
  (* Since the second step never ran, the collection [st1, st2] did not run *)
  let expected : Run_time.timestamps =
    Finished { ready = 12.; started = None; finished = 55. }
  in
  let result = Run_time.merge st1 st2 in
  Alcotest.(check timestamps) "merge_finished_finished" [ expected ] [ result ]

let test_timestamps_of_build () =
  let db = setup () in
  Alcotest.check database "Disk store initially empty" [] @@ [];
  let owner = "owner" in
  let name = "name" in
  let repo = { Ocaml_ci.Repo_id.owner; name } in
  let hash = "abc" in
  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build) \n\
     VALUES ('test', x'00', 'job1', x'01', 1, x'02', '2019-11-01 09:00', \
     '2019-11-01 09:01', '2019-11-01 09:02', 0)";
  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build) \n\
     VALUES ('test', x'01', 'job2', x'01', 0, x'21', '2019-11-01 09:03', \
     '2019-11-01 09:04', '2019-11-01 09:05', 0)";

  Index.record ~repo ~hash ~status:`Passed ~gref:"master"
    [ ("analysis", Some "job1"); ("alpine", Some "job2") ];

  (*
    Now the build consists of job1 ("analysis") that passed and job2 ("alpine"). Both steps ran and finished.

    Thus the resulting build should show
      queued_at:2019-11-01 09:00
      ran_at: 2019-11-01 09:01
      finished_at: 2019-11-01 09:05


    Note: 2019-11-01 09:00:00 is 1572598800 milliseconds from epoch
  *)
  let expected : Run_time.timestamps =
    Run_time.Finished
      {
        ready = 1572598800.;
        started = Some 1572598860.;
        finished = 1572599100.;
      }
  in
  let result = Option.get @@ Run_time.of_build ~owner ~name ~hash in
  Alcotest.(check timestamps) "timestamps_of_build" [ expected ] [ result ]

let test_timestamps_of_queued_build () =
  let db = setup () in
  Alcotest.check database "Disk store initially empty" [] @@ [];
  let owner = "owner" in
  let name = "name" in
  let repo = { Ocaml_ci.Repo_id.owner; name } in
  let hash = "abc" in
  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build) \n\
     VALUES ('test', x'00', 'job1', x'01', 1, x'02', '2019-11-01 09:00', \
     '2019-11-01 09:01', '2019-11-01 09:02', 0)";
  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, finished, \
     build) \n\
     VALUES ('test', x'01', 'job2', x'01', 0, x'21', '2019-11-01 09:03', \
     '2019-11-01 09:05', 0)";

  Index.record ~repo ~hash ~status:`Passed ~gref:"master"
    [ ("analysis", Some "job1"); ("alpine", Some "job2") ];

  (*
    The build consists of job1 ("analysis") that passed and job2 ("alpine"). job2 never ran.

    Thus the resulting build should show
      queued_at:2019-11-01 09:00

    Note: 2019-11-01 09:00:00 is 1572598800 milliseconds from epoch
  *)
  let expected : Run_time.timestamps = Run_time.Queued 1572598800. in
  let result = Option.get @@ Run_time.of_build ~owner ~name ~hash in
  Alcotest.(check timestamps) "timestamps_of_build" [ expected ] [ result ]

let test_timestamps_of_three_steps_build () =
  let db = setup () in
  Alcotest.check database "Disk store initially empty" [] @@ [];
  let owner = "owner" in
  let name = "name" in
  let repo = { Ocaml_ci.Repo_id.owner; name } in
  let hash = "abc" in
  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build) \n\
     VALUES ('test', x'00', 'job1', x'01', 1, x'02', '2019-11-01 09:00', \
     '2019-11-01 09:01', '2019-11-01 09:02', 0)";
  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build) \n\
     VALUES ('test', x'01', 'job2', x'01', 0, x'03', '2019-11-01 09:03', \
     '2019-11-01 09:04', '2019-11-01 09:05', 0)";
  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build) \n\
     VALUES ('test', x'02', 'job3', x'01', 0, x'04', '2019-11-01 09:02', \
     '2019-11-01 09:07', '2019-11-01 09:09', 0)";

  Index.record ~repo ~hash ~status:`Passed ~gref:"master"
    [
      ("analysis", Some "job1"); ("alpine", Some "job2"); ("debian", Some "job3");
    ];

  (*
    The build consists of job1, job2 and job3 and they have all passed.

    Thus the resulting build should show
      queued_at:2019-11-01 09:00
      started_at:2019-11-01 09:01
      finished_at:2019-11-01 09:09

    Note: 2019-11-01 09:00:00 is 1572598800 milliseconds from epoch
  *)
  let expected : Run_time.timestamps =
    Run_time.Finished
      {
        ready = 1572598800.;
        started = Some 1572598860.;
        finished = 1572599340.;
      }
  in
  let result = Option.get @@ Run_time.of_build ~owner ~name ~hash in
  Alcotest.(check timestamps) "timestamps_of_build" [ expected ] [ result ]

let tests =
  [
    Alcotest_lwt.test_case_sync "simple_run_times" `Quick test_simple_run_times;
    Alcotest_lwt.test_case_sync "running" `Quick test_running;
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
    Alcotest_lwt.test_case_sync "merge_queued_queued" `Quick
      test_merge_queued_queued;
    Alcotest_lwt.test_case_sync "merge_queued_running" `Quick
      test_merge_queued_running;
    Alcotest_lwt.test_case_sync "merge_queued_finished" `Quick
      test_merge_queued_finished;
    Alcotest_lwt.test_case_sync "merge_running_running" `Quick
      test_merge_running_running;
    Alcotest_lwt.test_case_sync "merge_running_finished" `Quick
      test_merge_running_finished;
    Alcotest_lwt.test_case_sync "merge_finished_finished" `Quick
      test_merge_finished_finished;
    Alcotest_lwt.test_case_sync "timestamps_of_build" `Quick
      test_timestamps_of_build;
    Alcotest_lwt.test_case_sync "timestamps_of_queued_build" `Quick
      test_timestamps_of_queued_build;
    Alcotest_lwt.test_case_sync "timestamps_of_queued_build" `Quick
      test_timestamps_of_three_steps_build;
  ]
