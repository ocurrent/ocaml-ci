let () =
  Unix.putenv "HOME" "/idontexist";
  (* Ignore user's git configuration *)
  Unix.putenv "GIT_AUTHOR_NAME" "test";
  Unix.putenv "GIT_COMMITTER_NAME" "test";
  Unix.putenv "EMAIL" "test@example.com";
  Lwt_main.run
  @@ Alcotest_lwt.run "ocaml-ci"
       [
         ("build_representation", Test_build_representation.tests);
         ("step_representation", Test_step_representation.tests);
       ]
