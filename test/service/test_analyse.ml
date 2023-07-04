open Lwt.Infix

let () =
  Logs.(set_level (Some Info));
  Logs.set_reporter @@ Logs_fmt.reporter ()

module Analysis = struct
  include Ocaml_ci.Analyse.Analysis

  type ocamlformat_source_type =
    | Opam of { version : string }
    | Vendored of { path : string }
  [@@deriving yojson, eq]

  let ocamlformat_source_type (t : t) =
    let osrc = ocamlformat_source t in
    Option.map
      (fun (osrc : Ocaml_ci.Analyse_ocamlformat.source) ->
        match osrc with
        | Opam { version; opam_repo_commit = _ } -> Opam { version }
        | Vendored { path } -> Vendored { path })
      osrc

  let set_equality = Alcotest.(equal (slist string String.compare))

  type selection = {
    ocaml_version : string;
    only_packages : string list; [@default []]
  }
  [@@deriving yojson, eq]

  let selection_type (t : t) =
    match selections t with
    | `Opam_monorepo _ -> "opam-monorepo"
    | `Opam_build _ -> "opam"

  let selection (t : Ocaml_ci.Selection.t) =
    let ocaml_version =
      Ocaml_version.to_string (Ocaml_ci.Variant.ocaml_version t.variant)
    in
    { ocaml_version; only_packages = t.only_packages }

  let selections (t : t) =
    match selections t with
    | `Opam_monorepo _ -> []
    | `Opam_build s -> List.map selection s

  (* Make the [t] type concrete from the observable fields for easier testing *)
  type t = {
    opam_files : string list; [@equal set_equality]
    selection_type : string;
    selections : selection list;
    ocamlformat_source_type : ocamlformat_source_type option;
  }
  [@@deriving eq, yojson]

  let of_dir ~job ~platforms ~opam_repository_commit d =
    let solver = Ocaml_ci.Backend_solver.v None in
    of_dir ~solver ~job ~platforms ~opam_repository_commit d
    |> Lwt_result.map (fun t ->
           {
             opam_files = opam_files t;
             selection_type = selection_type t;
             selections = selections t;
             ocamlformat_source_type = ocamlformat_source_type t;
           })

  let t : t Alcotest.testable =
    Alcotest.testable (Fmt.using to_yojson Yojson.Safe.pretty_print) equal
end

let unwrap_result ~job = function
  | Ok x -> x
  | Error (`Msg m) ->
      print_endline "---job log---";
      let id = Current.Job.id job in
      (match Current.Job.log_path id with
      | Error (`Msg m) -> print_endline m
      | Ok path ->
          let ch = open_in_bin (Fpath.to_string path) in
          let len = in_channel_length ch in
          print_endline @@ really_input_string ch len;
          close_in ch);
      print_endline "---end job log---";
      failwith m

let error_t = Alcotest.of_pp (fun f (`Msg m) -> Fmt.string f m)

let expect_test name ~project ~expected =
  Alcotest_lwt.test_case name `Quick (fun _switch () ->
      let ( // ) = Filename.concat in
      let src_dir = Filename.get_temp_dir_name () // "test-copy" in
      let root = src_dir // "_test" // name // "src" in
      let solver_dir = src_dir // "_test" // name in
      let repo = solver_dir // "opam-repository-builder" in
      let job =
        let label = "test_analyse-" ^ name in
        Current.Job.create
          ~switch:(Current.Switch.create ~label ())
          ~label ~config:(Current.Config.v ()) ()
      in
      Gen_project.instantiate ~root project;
      Gen_project.instantiate ~root:repo
        Gen_project.
          [
            folder "packages"
              [
                dummy_package "dune" [ "1.0" ];
                dummy_package "ocaml" [ "4.10.0"; "4.09.0" ];
                dummy_package "fmt" [ "1.0" ];
                dummy_package "logs" [ "1.0" ];
                dummy_package "alcotest" [ "1.0" ];
                dummy_package "opam-monorepo" [ "0.2.6" ];
                dummy_package "ocamlformat" [ "0.12" ];
              ];
          ];
      let opam_repository = Fpath.v repo in
      Current.Process.exec ~job ~cancellable:true ~cwd:opam_repository
        ("", [| "git"; "init" |])
      >|= unwrap_result ~job
      >>= fun () ->
      Current.Process.exec ~job ~cancellable:true ~cwd:opam_repository
        ("", [| "git"; "add"; "." |])
      >|= unwrap_result ~job
      >>= fun () ->
      Current.Process.exec ~job ~cancellable:true ~cwd:opam_repository
        ("", [| "git"; "commit"; "-m"; "init" |])
      >|= unwrap_result ~job
      >>= fun () ->
      Current.Process.check_output ~job ~cancellable:true ~cwd:opam_repository
        ("", [| "git"; "rev-parse"; "HEAD" |])
      >|= unwrap_result ~job
      >>= fun hash ->
      Current.Process.exec ~job ~cancellable:true ~cwd:(Fpath.v solver_dir)
        ( "",
          [|
            "git";
            "clone";
            "--bare";
            "opam-repository-builder";
            "opam-repository";
          |] )
      >|= unwrap_result ~job
      >>= fun () ->
      let opam_repository_commit =
        Current_git.Commit_id.v ~repo:"opam-repository" ~hash:(String.trim hash)
          ~gref:"master"
      in
      Analysis.of_dir ~job ~platforms:Test_platforms.v ~opam_repository_commit
        (Fpath.v root)
      >|= fun result ->
      (match (result, expected) with
      | Error _, Ok _ ->
          let path =
            Current.Job.(log_path (id job)) |> Result.get_ok |> Fpath.to_string
          in
          let ch = open_in_bin path in
          let len = in_channel_length ch in
          let log = really_input_string ch len in
          close_in ch;
          Printf.printf "Log:\n%s\n%!" log
      | _ -> ());
      Alcotest.(check (result Analysis.t error_t)) name expected result;
      Gc.full_major ())

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
    Ok
      {
        opam_files = [ "example.opam" ];
        selection_type = "opam";
        selections =
          [
            { ocaml_version = "4.10"; only_packages = [] };
            { ocaml_version = "4.09"; only_packages = [] };
          ];
        ocamlformat_source_type = Some (Opam { version = "0.12" });
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
    Ok
      {
        opam_files = [ "example.opam"; "example-foo.opam"; "example-bar.opam" ];
        selection_type = "opam";
        selections =
          [
            { ocaml_version = "4.10"; only_packages = [] };
            { ocaml_version = "4.09"; only_packages = [] };
          ];
        ocamlformat_source_type = None;
      }
  in
  expect_test "multiple_opam" ~project ~expected

(* There are two packages, but they support different versions of OCaml.
   Test each package on the platforms where it makes sense. *)
let test_filter_packages =
  let project =
    let open Gen_project in
    [
      File ("example.opam", opam);
      File ("example-new.opam", opam ~ocaml:{|{ >= "4.10" }|});
    ]
  in
  let expected =
    let open Analysis in
    Ok
      {
        opam_files = [ "example-new.opam"; "example.opam" ];
        selection_type = "opam";
        selections =
          [
            { ocaml_version = "4.10"; only_packages = [] };
            { ocaml_version = "4.09"; only_packages = [ "example.dev" ] };
          ];
        ocamlformat_source_type = None;
      }
  in
  expect_test "filter_packages" ~project ~expected

(* One packge doesn't work on *any* version of OCaml. Report that as an error. *)
let test_filter_packages_no_solution =
  let project =
    let open Gen_project in
    [
      File ("example.opam", opam);
      File ("example-new.opam", opam ~ocaml:{|{ >= "5.0" }|});
    ]
  in
  let expected =
    Error
      (`Msg
        {|No solution found for "example-new.dev" on any supported platform|})
  in
  expect_test "filter_packages_no_solution" ~project ~expected

let test_opam_monorepo =
  let project =
    let open Gen_project in
    [
      File ("example.opam", opam_monorepo_spec_file);
      File
        ( "example.opam.locked",
          opam_monorepo_lock_file ~monorepo_version:(Some "0.2") );
      File ("dune-project", empty_file);
    ]
  in
  let expected =
    let open Analysis in
    Ok
      {
        opam_files = [ "example.opam" ];
        selection_type = "opam-monorepo";
        selections = [];
        ocamlformat_source_type = None;
      }
  in
  expect_test "opam-monorepo" ~project ~expected

let test_opam_monorepo_no_version =
  let project =
    let open Gen_project in
    [
      File ("example.opam", opam_monorepo_spec_file);
      File
        ("example.opam.locked", opam_monorepo_lock_file ~monorepo_version:None);
      File ("dune-project", empty_file);
    ]
  in
  let expected =
    let open Analysis in
    Ok
      {
        opam_files = [ "example.opam" ];
        selection_type = "opam";
        selections = [ { ocaml_version = "4.10"; only_packages = [] } ];
        ocamlformat_source_type = None;
      }
  in
  expect_test "opam-monorepo-no-version" ~project ~expected

let test_ocamlformat_self =
  let project =
    let open Gen_project in
    [ File ("ocamlformat.opam", opam); File (".ocamlformat", empty_file) ]
  in
  let expected =
    let open Analysis in
    Ok
      {
        opam_files = [ "ocamlformat.opam" ];
        selection_type = "opam";
        selections =
          [
            { ocaml_version = "4.10"; only_packages = [] };
            { ocaml_version = "4.09"; only_packages = [] };
          ];
        ocamlformat_source_type = Some (Vendored { path = "." });
      }
  in
  expect_test "ocamlformat_self" ~project ~expected

let tests =
  [
    test_simple;
    test_multiple_opam;
    test_filter_packages;
    test_filter_packages_no_solution;
    test_opam_monorepo;
    test_opam_monorepo_no_version;
    test_ocamlformat_self;
  ]
