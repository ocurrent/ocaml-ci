let () =
  let open Alcotest in
  run "ocaml-ci" [ ("obuilder_specs", Test_spec.tests) ]
