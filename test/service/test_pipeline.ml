type t_in = ([ `Built | `Checked ], [ `Active of unit | `Msg of string ]) result
[@@deriving show, eq]

let t_in = Alcotest.testable pp_t_in equal_t_in

type t_ret = (unit, [ `Active of [ `Running ] | `Msg of string ]) result
[@@deriving show, eq]

let t_ret = Alcotest.testable pp_t_ret equal_t_ret

let test_summarise_success () =
  let result =
    [
      ("build_1", (Result.Ok `Built, ()));
      ("build_2", (Result.Ok `Built, ()));
      ("lint_1", (Result.Ok `Checked, ()));
    ]
    |> Ocaml_ci.Pipeline.summarise
  in
  let expected = Result.Ok () in
  Alcotest.(check t_ret) "Success" expected result

let test_summarise_fail () =
  let result =
    [
      ("build_1", (Result.Ok `Built, ()));
      ("build_2", (Result.Ok `Built, ()));
      ("lint_1", (Result.Ok `Checked, ()));
      ("build_3", (Result.Error (`Msg "msg"), ()));
    ]
    |> Ocaml_ci.Pipeline.summarise
  in
  let expected = Result.Error (`Msg "build_3 failed: msg") in
  Alcotest.(check t_ret) "Failed" expected result

let test_summarise_running () =
  let result =
    [
      ("build_1", (Result.Ok `Built, ()));
      ("build_2", (Result.Ok `Built, ()));
      ("lint_1", (Result.Ok `Checked, ()));
      ("lint_2", (Result.Error (`Active ()), ()));
    ]
    |> Ocaml_ci.Pipeline.summarise
  in
  let expected = Result.Error (`Active `Running) in
  Alcotest.(check t_ret) "Running" expected result

let test_summarise_success_experimental_fail () =
  let result =
    [
      ("build_1", (Result.Ok `Built, ()));
      ("build_2", (Result.Ok `Built, ()));
      ("lint_1", (Result.Ok `Checked, ()));
      ("macos-homebrew", (Result.Error (`Msg "failed"), ()));
    ]
    |> Ocaml_ci.Pipeline.summarise
  in
  let expected = Result.Ok () in
  Alcotest.(check t_ret) "Success" expected result

let test_summarise_success_experimental_running () =
  let result =
    [
      ("build_1", (Result.Ok `Built, ()));
      ("build_2", (Result.Ok `Built, ()));
      ("lint_1", (Result.Ok `Checked, ()));
      ("(lint-lower-bounds)", (Result.Error (`Active ()), ()));
    ]
    |> Ocaml_ci.Pipeline.summarise
  in
  let expected = Result.Ok () in
  Alcotest.(check t_ret) "Success" expected result

let tests =
  [
    Alcotest.test_case "summarise_success" `Quick test_summarise_success;
    Alcotest.test_case "summarise_fail" `Quick test_summarise_fail;
    Alcotest.test_case "summarise_running" `Quick test_summarise_running;
    Alcotest.test_case "summarise_success_experimental_fail" `Quick
      test_summarise_success_experimental_fail;
    Alcotest.test_case "summarise_success_experimental_running" `Quick
      test_summarise_success_experimental_running;
  ]
