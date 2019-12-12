open Lwt.Infix

module Analysis = struct
  include Ocaml_ci.Analyse.Analysis

  let set_equality = Alcotest.(equal (slist string String.compare))

  (* Make the [t] type concrete from the observable fields for easier testing *)
  type t = {
    opam_files : string list; [@equal set_equality]
    is_duniverse : bool;
    ocamlformat_version : ocamlformat_version option;
  }
  [@@deriving eq, yojson]

  let of_dir ~job d =
    of_dir ~job d
    |> Lwt_result.map (fun t ->
           {
             opam_files = opam_files t;
             is_duniverse = is_duniverse t;
             ocamlformat_version = ocamlformat_version t;
           })

  let t : t Alcotest.testable =
    Alcotest.testable (Fmt.using to_yojson Yojson.Safe.pretty_print) equal
end

let expect_test name ~project ~expected =
  Alcotest_lwt.test_case name `Quick (fun _switch () ->
      let ( // ) = Filename.concat in
      let root = Filename.current_dir_name // "_test" // name in
      let job =
        let label = "test_analyse-" ^ name in
        Current.Job.create
          ~switch:(Current.Switch.create ~label ())
          ~label ~config:(Current.Config.v ()) ()
      in
      let () = Gen_project.instantiate ~root project in
      Analysis.of_dir ~job (Fpath.v root)
      >|= (function
            | Ok o -> o
            | Error (`Msg e) -> Alcotest.failf "Analysis stage failed: %s" e)
      >|= Alcotest.(check Analysis.t) name expected)

(* example duniverse containing a single package *)
let duniverse =
  let open Gen_project in
  Folder
    ( "duniverse",
      [ Folder ("alcotest.0.8.5", [ File ("alcotest.opam", opam) ]) ] )

let test_simple =
  let project =
    let open Gen_project in
    [
      File ("example.opam", opam);
      File (".ocamlformat", ocamlformat ~version:"0.12");
    ]
  in
  let expected =
    let open Analysis in
    {
      opam_files = [ "example.opam" ];
      is_duniverse = false;
      ocamlformat_version = Some (Version "0.12");
    }
  in
  expect_test "simple" ~project ~expected

let test_multiple_opam =
  let project =
    let open Gen_project in
    [
      File ("example.opam", opam);
      File ("example-foo.opam", opam);
      File ("example-bar.opam", opam);
      Folder
        ( "test",
          [
            (* .opam files not in the top-level of the project should be ignored *)
            File ("ignored.opam", opam);
            (* vendored duniverse should not be attributed to the project
               (including internal .opam files) *)
            Folder ("vendored", [ duniverse ]);
          ] );
    ]
  in
  let expected =
    let open Analysis in
    {
      opam_files = [ "example.opam"; "example-foo.opam"; "example-bar.opam" ];
      is_duniverse = false;
      ocamlformat_version = None;
    }
  in
  expect_test "multiple_opam" ~project ~expected

let test_duniverse =
  let project =
    let open Gen_project in
    [ File ("example.opam", opam); duniverse ]
  in
  let expected =
    let open Analysis in
    {
      opam_files = [ "example.opam"; "duniverse/alcotest.0.8.5/alcotest.opam" ];
      is_duniverse = true;
      ocamlformat_version = None;
    }
  in
  expect_test "duniverse" ~project ~expected

let test_ocamlformat_vendored =
  let project =
    let open Gen_project in
    [
      File ("example.opam", opam);
      (* This file is not parsed if ocamlformat is vendored *)
      File (".ocamlformat", empty_file);
      Folder
        ( "duniverse",
          [ Folder ("ocamlformat", [ File ("ocamlformat.opam", opam) ]) ] );
    ]
  in
  let expected =
    let open Analysis in
    {
      opam_files = [ "example.opam"; "duniverse/ocamlformat/ocamlformat.opam" ];
      is_duniverse = true;
      ocamlformat_version = Some Vendored;
    }
  in
  expect_test "ocamlformat_vendored" ~project ~expected

let test_ocamlformat_self =
  let project =
    let open Gen_project in
    [ File ("ocamlformat.opam", opam); File (".ocamlformat", empty_file) ]
  in
  let expected =
    let open Analysis in
    {
      opam_files = [ "ocamlformat.opam" ];
      is_duniverse = false;
      ocamlformat_version = Some Vendored;
    }
  in
  expect_test "ocamlformat_self" ~project ~expected

let tests =
  [
    test_simple;
    test_multiple_opam;
    test_duniverse;
    test_ocamlformat_vendored;
    test_ocamlformat_self;
  ]
