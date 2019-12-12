let () =
  Alcotest.run "ocaml-ci" [
    ("index", Test_index.tests);
    ("analyse", Test_analyse.tests)
  ]
