let normal_tests () =
  Alcotest.run "ocaml-ci"
    [
      ("obuilder_specs", Test_spec.tests);
      ("pipeline", Test_pipeline.tests);
      ("conf", Test_conf.tests);
    ]

let lwt_tests () =
  Unix.putenv "HOME" "/idontexist";
  (* Ignore user's git configuration *)
  Unix.putenv "GIT_AUTHOR_NAME" "test";
  Unix.putenv "GIT_COMMITTER_NAME" "test";
  Unix.putenv "EMAIL" "test@example.com";
  Lwt_main.run
  @@ Alcotest_lwt.run "ocaml-ci"
       [
         ("index", Test_index.tests);
         ("analyse", Test_analyse.tests);
         ("run_time", Test_run_time.tests);
       ]

let () =
  normal_tests ();
  lwt_tests ()
