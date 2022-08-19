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

  let expected = [ ("master", hash) ] in
  let result = Index.get_active_refs repo |> Ref_map.bindings in
  Alcotest.(check (list (pair string string))) "Refs" expected result

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
     VALUES ('test', x'00', 'job1', x'01', 1, x'02', '2019-11-01 09:00', \
     '2019-11-01 09:01', '2019-11-01 09:02', 0)";
  Index.record ~repo ~hash ~status:`Pending ~gref:"master"
    [ ("analysis", Some "job1"); ("alpine", None) ];
  let expected = [ ("alpine", `Not_started); ("analysis", `Passed) ] in
  let result = Index.get_jobs ~owner ~name hash in
  Alcotest.(check jobs) "Jobs" expected result;

  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build) \n\
     VALUES ('test', x'01', 'job2', x'01', 0, x'21', '2019-11-01 09:03', \
     '2019-11-01 09:04', '2019-11-01 09:05', 0)";
  Index.record ~repo ~hash ~status:`Failed ~gref:"master"
    [ ("analysis", Some "job1"); ("alpine", Some "job2") ];
  let expected = [ ("alpine", `Failed "!"); ("analysis", `Passed) ] in
  let result = Index.get_jobs ~owner ~name hash in
  Alcotest.(check jobs) "Jobs" expected result;

  Index.record ~repo ~hash ~status:`Passed ~gref:"master"
    [ ("analysis", Some "job1") ];
  let expected = [ ("analysis", `Passed) ] in
  let result = Index.get_jobs ~owner ~name hash in
  Alcotest.(check jobs) "Jobs" expected result

let test_get_build_history () =
  let owner = "owner" in
  let name = "name" in
  let repo = { Ocaml_ci.Repo_id.owner; name } in
  let hash = "abc" in
  Index.record ~repo ~hash ~status:`Failed ~gref:"master"
    [ ("analysis", Some "job1"); ("alpine", Some "job2") ];
  Index.record ~repo ~hash ~status:`Passed ~gref:"master"
    [ ("analysis", Some "job1") ];
  Index.record ~repo ~hash:"def" ~status:`Passed ~gref:"master"
    [ ("lint", Some "job2") ];
  let expected =
    [ ("analysis", "abc", Some "job1"); ("lint", "def", Some "job2") ]
  in
  let result = Index.get_build_history ~owner ~name ~gref:"master" in
  Alcotest.(check commits_jobs) "Commits" expected result

let tests =
  [
    Alcotest_lwt.test_case_sync "build_history" `Quick test_get_build_history;
    Alcotest_lwt.test_case_sync "active_refs" `Quick test_active_refs;
    Alcotest_lwt.test_case_sync "jobs" `Quick test_get_jobs;
  ]
