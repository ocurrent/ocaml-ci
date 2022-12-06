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
    match (ts1, ts2) with
    | None, None -> v1 = v2 && s1 = s2
    | None, Some _ | Some _, None -> false
    | Some ts1, Some ts2 -> v1 = v2 && s1 = s2 && Run_time.eq_timestamps ts1 ts2
  in
  Alcotest.testable (Fmt.Dump.list state) (List.equal equal)

let commit_state_status =
  let state f (cs : Index.Commit_cache.commit_state) =
    let s = Index.Commit_cache.get_status cs in
    Fmt.pf f "%a" Index.pp_status s
  in
  let equal cs1 cs2 = cs1.Index.Commit_cache.s = cs2.Index.Commit_cache.s in
  Alcotest.testable (Fmt.Dump.list state) (List.equal equal)

let database = Alcotest.(list string)

let setup () =
  let open Lwt.Syntax in
  let+ () = Index.init () in
  let db = Lazy.force Current.Db.v in
  Current.Db.exec_literal db "DELETE FROM cache";
  db

let ref_info =
  Alcotest.testable (fun f state -> Fmt.pf f "%a" Index.pp_ref_info state) ( = )

let test_active_refs () =
  let owner = "owner" in
  let name = "name" in
  let repo = { Ocaml_ci.Repo_id.owner; name } in
  let hash = "abc" in
  let message = "message" in
  let name = "name" in
  let refs = Ref_map.singleton "master" { Index.hash; message; name } in
  Index.set_active_refs ~repo refs "master";

  let expected = [ ("master", { Index.hash; message; name }) ] in
  let result = Index.get_active_refs repo |> Ref_map.bindings in
  Alcotest.(check (list (pair string ref_info))) "Refs" expected result

let test_get_jobs _switch () =
  let open Lwt.Syntax in
  let+ db = setup () in
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
  let job_1_ts : Run_time.timestamps =
    Run_time.Finished
      {
        queued_at = 1572598800.;
        started_at = Some 1572598860.;
        finished_at = 1572598920.;
      }
  in
  let expected =
    [ ("alpine", `Not_started, None); ("analysis", `Passed, Some job_1_ts) ]
  in
  let result = Index.get_jobs ~owner ~name hash in
  Alcotest.(check jobs) "Jobs" expected result;

  Current.Db.exec_literal db
    "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, \
     finished, build) \n\
     VALUES ('test', x'01', 'job2', x'01', 0, x'21', '2019-11-01 09:03', \
     '2019-11-01 09:04', '2019-11-01 09:05', 0)";
  Index.record ~repo ~hash ~status:`Failed ~gref:"master"
    [ ("analysis", Some "job1"); ("alpine", Some "job2") ];
  let job_2_ts : Run_time.timestamps =
    Run_time.Finished
      {
        queued_at = 1572598980.;
        started_at = Some 1572599040.;
        finished_at = 1572599100.;
      }
  in
  let expected =
    [
      ("alpine", `Failed "!", Some job_2_ts);
      ("analysis", `Passed, Some job_1_ts);
    ]
  in
  let result = Index.get_jobs ~owner ~name hash in
  Alcotest.(check jobs) "Jobs" expected result;

  Index.record ~repo ~hash ~status:`Passed ~gref:"master"
    [ ("analysis", Some "job1") ];
  let expected = [ ("analysis", `Passed, Some job_1_ts) ] in
  let result = Index.get_jobs ~owner ~name hash in
  Alcotest.(check jobs) "Jobs" expected result

let test_get_build_history _ () =
  let open Lwt.Syntax in
  let+ _ = setup () in
  let owner = "owner" in
  let name = "name" in
  let repo = { Ocaml_ci.Repo_id.owner; name } in
  let hash = "abc" in
  Index.record ~repo ~hash ~status:`Failed ~gref:"master"
    [
      ("analysis", Some "job1"); ("lint", Some "job2"); ("alpine", Some "job2");
    ];
  let expected : Index.Commit_cache.commit_state =
    { s = `Failed; started_at = None; ran_for = None }
  in
  let build_summary = Index.get_build_history ~owner ~name ~gref:"master" in
  let result =
    List.map
      (fun ( hash,
             build_number,
             status,
             started_at,
             total_ran_for,
             ran_for,
             total_queued_for,
             _message ) ->
        Index.Commit_cache.commit_state_from_build_summary ~hash ~build_number
          ~status ~started_at ~total_ran_for ~ran_for ~total_queued_for)
      build_summary
  in
  Alcotest.(check commit_state_status) "Commits" [ expected ] result

let tests =
  [
    Alcotest_lwt.test_case "build_history" `Quick test_get_build_history;
    Alcotest_lwt.test_case_sync "active_refs" `Quick test_active_refs;
    Alcotest_lwt.test_case "jobs" `Quick test_get_jobs;
  ]
