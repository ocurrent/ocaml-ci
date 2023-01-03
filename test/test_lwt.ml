let () =
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
         ("variant", Test_variant.tests);
         ("run_time", Test_run_time.tests);
         ("run_time_client", Test_run_time_client.tests);
         ("build_representation", Test_build_representation.tests);
         ("step_representation", Test_step_representation.tests);
       ]