module Index = Ocaml_ci.Index
module Ref_map = Index.Ref_map

let jobs =
  let state f (variant, state) =
    Fmt.pf f "%s:%a" variant Index.pp_job_state state
  in
  Alcotest.testable (Fmt.Dump.list state) ( = )

let commits_jobs =
  let state f (variant, hash, job_id) =
    Fmt.pf f "%s:%s %a@" variant hash Fmt.(Dump.option string) job_id
  in
  Alcotest.testable (Fmt.Dump.list state) ( = )

let database = Alcotest.(list string)

let setup () =
  let db = Lazy.force Current.Db.v in
  Index.init ();
  Current.Db.exec_literal db "DELETE FROM cache";
  db

let test_active_refs () =
  let owner = "owner" in
  let name = "name" in
  let repo = { Ocaml_ci.Repo_id.owner; name } in
  let hash = "abc" in
  Index.set_active_refs ~repo @@ Ref_map.singleton "master" hash;
  Alcotest.(check (list (pair string string)))
    "Refs"
    [ ("master", hash) ]
    (Index.get_active_refs repo |> Ref_map.bindings)

let test_get_jobs () =
  let db = setup () in
  Alcotest.check database "Disk store initially empty" [] @@ [];
  let owner = "owner" in
  let name = "name" in
  let repo = { Ocaml_ci.Repo_id.owner; name } in
  let hash = "abc" in
  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build) \n\
     VALUES ('test', x'00', 'job1', x'01', 1, x'02', '2019-11-01 9:00', \
     '2019-11-01 9:01', '2019-11-01 9:02', 0)";
  Index.record ~repo ~hash ~status:`Pending ~gref:"master"
    [ ("analysis", Some "job1"); ("alpine", None) ];
  Alcotest.(check jobs)
    "Jobs"
    [ ("alpine", `Not_started); ("analysis", `Passed) ]
  @@ Index.get_jobs ~owner ~name hash;
  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build) \n\
     VALUES ('test', x'01', 'job2', x'01', 0, x'21', '2019-11-01 9:03', \
     '2019-11-01 9:04', '2019-11-01 9:05', 0)";
  Index.record ~repo ~hash ~status:`Failed ~gref:"master"
    [ ("analysis", Some "job1"); ("alpine", Some "job2") ];
  Alcotest.(check jobs)
    "Jobs"
    [ ("alpine", `Failed "!"); ("analysis", `Passed) ]
  @@ Index.get_jobs ~owner ~name hash;
  Index.record ~repo ~hash ~status:`Passed ~gref:"master"
    [ ("analysis", Some "job1") ];
  Alcotest.(check jobs) "Jobs" [ ("analysis", `Passed) ]
  @@ Index.get_jobs ~owner ~name hash

let test_get_build_history () =
  let db = setup () in
  Alcotest.check database "Disk store initially empty" [] @@ [];
  let owner = "owner" in
  let name = "name" in
  let repo = { Ocaml_ci.Repo_id.owner; name } in
  let hash = "abc" in
  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build)\n\
     VALUES ('test', x'00', 'job1', x'01', 1, x'02', '2019-11-01 9:00', \
     '2019-11-01 9:01', '2019-11-01 9:02', 0)";
  Index.record ~repo ~hash ~status:`Pending ~gref:"master"
    [ ("analysis", Some "job1"); ("alpine", None) ];
  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build)\n\
     VALUES ('test', x'01', 'job2', x'01', 0, x'21', '2019-11-01 9:03', \
     '2019-11-01 9:04', '2019-11-01 9:05', 0)";
  Index.record ~repo ~hash ~status:`Failed ~gref:"master"
    [ ("analysis", Some "job1"); ("alpine", Some "job2") ];
  Index.record ~repo ~hash ~status:`Passed ~gref:"master"
    [ ("analysis", Some "job1") ];
  Index.record ~repo ~hash:"def" ~status:`Passed ~gref:"master"
    [ ("lint", Some "job2") ];
  Alcotest.(check commits_jobs)
    "Commits"
    [ ("analysis", "abc", Some "job1"); ("lint", "def", Some "job2") ]
  @@ Index.get_build_history ~owner ~name ~gref:"master"

let tests =
  [
    Alcotest_lwt.test_case_sync "build_history" `Quick test_get_build_history;
    Alcotest_lwt.test_case_sync "active_refs" `Quick test_active_refs;
    Alcotest_lwt.test_case_sync "jobs" `Quick test_get_jobs;
  ]
