let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "ocaml-ci"
       [ ("index", Test_index.tests); ("analyse", Test_analyse.tests) ]
