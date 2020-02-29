module Index = Ocaml_ci.Index

let jobs =
  let state f (variant, state) = Fmt.pf f "%s:%a" variant Index.pp_job_state state in
  Alcotest.testable (Fmt.Dump.list state) (=)

let test_simple () =
  Alcotest.(check (list string)) "Empty DB" [] @@ Index.list_owners ();
  let owner = "owner" in
  let name = "name" in
  let repo = { Current_github.Repo_id.owner; name } in
  let hash = "abc" in
  let db = Lazy.force Current.Db.v in
  Current.Db.exec_literal db "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, finished, build)
                                     VALUES ('test', x'00', 'job1', x'01', 1, x'02', '2019-11-01 9:00', '2019-11-01 9:01', '2019-11-01 9:02', 0)";
  Index.set_active_refs ~repo ["master", hash];
  Index.record ~repo ~hash ~status:`Pending [ "analysis", Some "job1";
                             "alpine", None ];
  Alcotest.(check (list string)) "Owners" ["owner"] @@ Index.list_owners ();
  Alcotest.(check (list string)) "Repos" ["name"] @@ Index.list_repos owner;
  Alcotest.(check (list (pair string string))) "Refs" ["master", hash] @@ Index.get_active_refs repo;
  Alcotest.(check jobs) "Jobs" ["alpine", `Not_started; "analysis", `Passed] @@ Index.get_jobs ~owner ~name hash;
  Current.Db.exec_literal db "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, finished, build)
                                     VALUES ('test', x'01', 'job2', x'01', 0, x'21', '2019-11-01 9:03', '2019-11-01 9:04', '2019-11-01 9:05', 0)";
  Index.record ~repo ~hash ~status:`Failed [ "analysis", Some "job1";
                             "alpine", Some "job2" ];
  Alcotest.(check jobs) "Jobs" ["alpine", `Failed "!"; "analysis", `Passed] @@ Index.get_jobs ~owner ~name hash;
  Index.record ~repo ~hash ~status:`Passed [ "analysis", Some "job1" ];
  Alcotest.(check jobs) "Jobs" ["analysis", `Passed] @@ Index.get_jobs ~owner ~name hash

let tests = [
    Alcotest_lwt.test_case_sync "simple" `Quick test_simple;
  ]
