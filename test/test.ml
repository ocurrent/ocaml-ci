let () =
  let open Alcotest in
  run "ocaml-ci" [ ("macos_obuilder_specs", Test_spec.tests) ]
