let () =
  Unix.putenv "HOME" "/idontexist";
  (* Ignore user's git configuration *)
  Unix.putenv "GIT_AUTHOR_NAME" "test";
  Unix.putenv "GIT_COMMITTER_NAME" "test";
  Unix.putenv "EMAIL" "test@example.com";
  Alcotest.run "ocaml-ci" [ ("obuilder_specs", Test_spec.tests) ]
