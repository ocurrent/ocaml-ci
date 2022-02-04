let test_one s =
  let t = Ocaml_ci.Variant.of_string s in
  let s' = Ocaml_ci.Variant.to_string t in
  Alcotest.(check string) "encode/decode" s s'

let test_error s =
  try
    let _ = Ocaml_ci.Variant.of_string s in
    Alcotest.failf "%s: should have failed" s
  with Failure _ | Invalid_argument _ ->
    ()

let test_simple () =
  List.iter test_one [
    "debian-11-4.13_x86_32_opam-2.1";
    "debian-11-4.13_opam-2.1";
    "debian-11-4.13_x86_32";
    ]

let test_errors () =
  List.iter test_error [
    "what-11_x86_32_opam-2.1";
    "what_x86_32_opam-2.1";
    "debian-11-4.13_opam-3.1";
    "debian-11-4.13_x87_32";
    ]

let tests = [
  Alcotest_lwt.test_case_sync "simple" `Quick test_simple;
  Alcotest_lwt.test_case_sync "errors" `Quick test_errors;
]
