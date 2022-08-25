module Index = Ocaml_ci.Index
module Ref_map = Index.Ref_map
module Run_time = Ocaml_ci.Run_time


let jobs =
  let state f (variant, state, (ts : Run_time.timestamps option)) =
    let ts' =
    match ts with
    | None -> ""
    | Some ts -> Fmt.str "%a" Run_time.pp_timestamps ts
    in
    Fmt.pf f "%s:%a:%s" variant Index.pp_job_state state ts'
  in
  let equal (v1, s1, ts1) (v2, s2, ts2) =
    match ts1, ts2 with
    | None, None -> v1=v2 && s1=s2
    | None, Some _ | Some _, None -> false
    | Some ts1, Some ts2 -> v1 = v2 && s1 = s2 && Run_time.eq_timestamps ts1 ts2
  in
  Alcotest.testable (Fmt.Dump.list state) (List.equal equal)

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
  Index.record ~repo ~hash ~status:`Pending ~gref:"master" [ ("analysis", Some "job1"); ("alpine", None) ];
  let job_1_ts : Run_time.timestamps = Run_time.Finished { ready=1572598800.;
                                                           started=Some 1572598860.;
                                                           finished=1572598920. } in
  let expected = [ ("alpine", `Not_started, None); ("analysis", `Passed, Some job_1_ts) ] in
  let result = Index.get_jobs ~owner ~name hash in
  Alcotest.(check jobs) "Jobs" expected result;

  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build) \n\
     VALUES ('test', x'01', 'job2', x'01', 0, x'21', '2019-11-01 09:03', \
     '2019-11-01 09:04', '2019-11-01 09:05', 0)";
  Index.record ~repo ~hash ~status:`Failed ~gref:"master"
    [ ("analysis", Some "job1"); ("alpine", Some "job2") ];
  let job_2_ts : Run_time.timestamps = Run_time.Finished { ready=1572598980.;
                                                           started=Some 1572599040.;
                                                           finished=1572599100. } in
  let expected = [ ("alpine", `Failed "!", Some job_2_ts); ("analysis", `Passed, Some job_1_ts) ] in
  let result = Index.get_jobs ~owner ~name hash in
  Alcotest.(check jobs) "Jobs" expected result;

  Index.record ~repo ~hash ~status:`Passed ~gref:"master"
    [ ("analysis", Some "job1") ];
  let expected = [ ("analysis", `Passed, Some job_1_ts) ] in
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
