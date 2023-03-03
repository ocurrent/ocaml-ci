module Index = Ocaml_ci.Index
module Run_time = Ocaml_ci.Run_time
module Job = Current.Job
module Client = Ocaml_ci_api.Client

let setup () =
  let open Lwt.Syntax in
  let+ () = Index.init () in
  let db = Lazy.force Current.Db.v in
  Current.Db.exec_literal db "DELETE FROM cache";
  db

let database = Alcotest.(list string)
let cmp_floats v1 v2 = abs_float (v1 -. v2) < 0.0000001

let timestamps =
  let state f (st : Run_time.Timestamp.t) =
    Fmt.pf f "%a" Run_time.Timestamp.pp st
  in
  Alcotest.testable (Fmt.Dump.list state) (List.equal Run_time.Timestamp.eq)

let test_running _switch () =
  let open Lwt.Syntax in
  (Job.timestamp := fun () -> 0.1);
  (* Note that this is what is used for the starting-at timestamp *)
  let switch = Current.Switch.create ~label:"output" () in
  let config = Current.Config.v () in
  let pool = Current.Pool.create ~label:"test" 1 in
  let job1 = Job.create ~switch ~label:"output" ~config () in
  let _ = Job.start ~pool ~level:Current.Level.Harmless job1 in
  let+ db = setup () in
  Alcotest.check database "Disk store initially empty" [] @@ [];
  Current.Db.exec_literal db
    (Fmt.str
       "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, \
        running, finished, build) \n\
        VALUES ('test42', x'00', '%s', x'01', 1, x'02', '1970-01-01 00:00', \
        '1970-01-01 00:01', '1970-01-01 00:00', 0)"
       (Job.id job1));
  let expected : Run_time.Timestamp.t =
    Running { queued_at = 0.; started_at = 0.1 }
  in
  let result = Option.get (Run_time.Timestamp.of_job_id_opt @@ Job.id job1) in
  Alcotest.(check timestamps) "Running" [ expected ] [ result ]

let test_simple_run_times _switch () =
  let open Lwt.Syntax in
  let+ db = setup () in
  Alcotest.check database "Disk store initially empty" [] @@ [];
  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build) \n\
     VALUES ('test42', x'00', 'job42', x'01', 1, x'02', '2019-11-01 09:00', \
     '2019-11-01 09:05', '2019-11-01 10:04', 0)";
  (* 2019-11-01 09:00:00 is 1572598800 seconds from epoch *)
  let expected : Run_time.Timestamp.t =
    Finished
      {
        queued_at = 1572598800.;
        started_at = Some 1572599100.;
        finished_at = 1572602640.;
      }
  in
  let result = Option.get (Run_time.Timestamp.of_job_id_opt "job42") in
  Alcotest.(check timestamps) "Finished" [ expected ] [ result ];

  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, finished, \
     build) \n\
     VALUES ('test43', x'00', 'job43', x'01', 1, x'02', '2019-11-01 09:00', \
     '2019-11-01 10:04', 0)";
  let expected = Run_time.Timestamp.Queued 1572598800. in
  let result = Option.get (Run_time.Timestamp.of_job_id_opt "job43") in
  Alcotest.(check timestamps) "Queued" [ expected ] [ result ]

let tests =
  [
    Alcotest_lwt.test_case "simple_run_times" `Quick test_simple_run_times;
    Alcotest_lwt.test_case "running" `Quick test_running;
  ]
