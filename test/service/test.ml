let () =
  Alcotest.run "ocaml-ci"
    [ ("obuilder_specs", Test_spec.tests); ("pipeline", Test_pipeline.tests) ]
