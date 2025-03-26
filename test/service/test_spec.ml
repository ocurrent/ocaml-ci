(* Expected obuilder spec for macos build of bondi. *)
let expected_macos_spec =
  {|
((from macos-homebrew-ocaml-4.14)
 (comment macos-homebrew-4.14.0_opam-2.1)
 (user (uid 1000) (gid 1000)) (env CLICOLOR_FORCE 1)
 (env OPAMCOLOR always)
 (run (shell "ln -f ~/local/bin/opam-2.1 ~/local/bin/opam"))
 (run (shell "opam init --reinit -ni"))
 (run (shell "uname -rs && opam exec -- ocaml -version && opam --version"))
 (run (cache (opam-archives (target /Users/mac1000/.opam/download-cache))
       (homebrew (target /Users/mac1000/Library/Caches/Homebrew)))
      (network host)
      (shell "cd ~/opam-repository && (git cat-file -e f207d3f018d642d1fcddb2c118e7fa8e65f4e366 || git fetch origin master) && git reset -q --hard f207d3f018d642d1fcddb2c118e7fa8e65f4e366 && git log --no-decorate -n1 --oneline && opam update -u"))
 (copy (src bondi.opam) (dst ./src/./))
 (run (network host)
      (shell "opam pin add -yn bondi.dev './src/./'"))
 (run (network host)
      (shell "echo '(lang dune 3.0)' > './src/./dune-project'"))
 (env DEPS "base-bigarray.base base-threads.base base-unix.base dune.3.6.0 menhir.20220210 menhirLib.20220210 menhirSdk.20220210 ocaml.4.14.0 ocaml-base-compiler.4.14.0 ocaml-config.2 ocaml-options-vanilla.1")
 (env CI true)
 (env OCAMLCI true)
 (run (cache (opam-archives (target /Users/mac1000/.opam/download-cache))
       (homebrew (target /Users/mac1000/Library/Caches/Homebrew)))
      (network host)
      (shell "opam update --depexts && opam install --cli=2.1 --depext-only -y bondi.dev $DEPS"))
 (run (cache (opam-archives (target /Users/mac1000/.opam/download-cache))
       (homebrew (target /Users/mac1000/Library/Caches/Homebrew)))
      (network host)
      (shell "opam install $DEPS"))
 (copy (src .) (dst ./src))
 (run (shell "cd ./src && opam exec -- dune build @install @check @runtest && rm -rf _build"))
)
           |}

(* Expected obuilder spec for linux (debian-11) build of bondi. *)
let expected_linux_spec =
  {|
((from ocaml/opam@sha256:03668731d460043acc763d35e1d5dfc6e6fe68a02f987849ac74f855e3e42c10)
 (comment debian-11-4.14.0_opam-2.1)
 (user (uid 1000) (gid 1000)) (env CLICOLOR_FORCE 1)
 (env OPAMCOLOR always)
 (workdir /src)
 (run (shell "sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam"))
 (run (shell "opam init --reinit -ni"))
 (run (shell "uname -rs && opam exec -- ocaml -version && opam --version"))
 (workdir /src)
 (run (shell "sudo chown opam /src"))
 (run (cache (opam-archives (target /home/opam/.opam/download-cache)))
      (network host)
      (shell "cd ~/opam-repository && (git cat-file -e f207d3f018d642d1fcddb2c118e7fa8e65f4e366 || git fetch origin master) && git reset -q --hard f207d3f018d642d1fcddb2c118e7fa8e65f4e366 && git log --no-decorate -n1 --oneline && opam update -u"))
 (copy (src bondi.opam) (dst ./))
 (run (network host)
      (shell "opam pin add -yn bondi.dev './'"))
 (run (network host)
      (shell "echo '(lang dune 3.0)' > './dune-project'"))
 (env DEPS "base-bigarray.base base-threads.base base-unix.base dune.3.6.0 menhir.20220210 menhirLib.20220210 menhirSdk.20220210 ocaml.4.14.0 ocaml-base-compiler.4.14.0 ocaml-config.2 ocaml-options-vanilla.1")
 (env CI true)
 (env OCAMLCI true)
 (run (cache (opam-archives (target /home/opam/.opam/download-cache)))
      (network host)
      (shell "opam update --depexts && opam install --cli=2.1 --depext-only -y bondi.dev $DEPS"))
 (run (cache (opam-archives (target /home/opam/.opam/download-cache)))
      (network host)
      (shell "opam install $DEPS"))
 (copy (src .) (dst /src))
 (run (shell "opam exec -- dune build @install @check @runtest && rm -rf _build"))
)
|}

(* Expected obuilder spec for windows server 2022 build of bondi. *)
let expected_windows_spec =
  {|
((from windows-server-2022-amd64-ocaml-4.14)
 (comment windows-server-2022-4.14.0_opam-2.3)
 (user (uid 1000) (gid 1000)) (env CLICOLOR_FORCE 1)
 (env OPAMCOLOR always)
 (workdir /src)
 (run (shell "sudo ln -f /usr/bin/opam-2.3 /usr/bin/opam"))
 (run (shell "opam init --reinit -ni"))
 (run (shell "uname -rs && opam exec -- ocaml -version && opam --version"))
 (workdir /src)
 (run (shell "sudo chown opam /src"))
 (run (cache (opam-archives (target /home/opam/.opam/download-cache)))
      (network host) (shell "cd ~/opam-repository && (git cat-file -e f207d3f018d642d1fcddb2c118e7fa8e65f4e366 || git fetch origin master) && git reset -q --hard f207d3f018d642d1fcddb2c118e7fa8e65f4e366 && git log --no-decorate -n1 --oneline && opam update -u"))
 (copy (src bondi.opam) (dst ./))
 (run (network host) (shell "opam pin add -yn bondi.dev './'"))
 (run (network host)
      (shell "echo '(lang dune 3.0)' > './dune-project'"))
 (env DEPS "arch-x86_64.1 base-bigarray.base base-threads.base base-unix.base conf-mingw-w64-gcc-x86_64.1 dune.3.17.0 flexdll.0.43 host-arch-x86_64.1 host-system-mingw.1 menhir.20240715 menhirCST.20240715 menhirLib.20240715 menhirSdk.20240715 mingw-w64-shims.0.2.0 ocaml.4.14.2 ocaml-base-compiler.4.14.2 ocaml-config.3 ocaml-env-mingw64.1 ocaml-options-vanilla.1 system-mingw.1")
 (env CI true)
 (env OCAMLCI true)
 (run (cache (opam-archives (target /home/opam/.opam/download-cache)))
      (network host)
      (shell "opam update --depexts && opam install --cli=2.3 --depext-only -y bondi.dev $DEPS"))
 (run (cache
      (opam-archives (target /home/opam/.opam/download-cache)))
      (network host)
      (shell "opam install $DEPS"))
 (copy (src .) (dst /src))
 (run (shell "opam exec -- dune build @install @check @runtest && rm -rf _build"))
)
|}

(* Create testable Sexp for Alcotest. *)
let sexp = Alcotest.testable Sexplib0__Sexp.pp_hum Sexplib0__Sexp.equal

let test_macos_spec () =
  let open Ocaml_ci in
  let expected = Sexplib__Pre_sexp.of_string expected_macos_spec in
  let variant =
    Variant.v ~arch:`X86_64 ~distro:"macos-homebrew"
      ~ocaml_version:Ocaml_version.Releases.v4_14_0 ~opam_version:`V2_1
    |> Result.get_ok
  in
  let actual =
    Opam_build.spec ~base:"macos-homebrew-ocaml-4.14" ~opam_version:`V2_1
      ~opam_files:[ "bondi.opam" ]
      ~selection:
        Selection.
          {
            variant;
            packages =
              [
                "bondi.dev";
                "base-bigarray.base";
                "base-threads.base";
                "base-unix.base";
                "dune.3.6.0";
                "menhir.20220210";
                "menhirLib.20220210";
                "menhirSdk.20220210";
                "ocaml.4.14.0";
                "ocaml-base-compiler.4.14.0";
                "ocaml-config.2";
                "ocaml-options-vanilla.1";
              ];
            only_packages = [];
            commit = "f207d3f018d642d1fcddb2c118e7fa8e65f4e366";
            lower_bound = false;
          }
    |> Obuilder_spec.sexp_of_t
  in
  Alcotest.(check sexp) "validate" expected actual

let test_linux_spec () =
  let open Ocaml_ci in
  let expected = Sexplib__Pre_sexp.of_string expected_linux_spec in
  let variant =
    Variant.v ~arch:`X86_64 ~distro:"debian-11"
      ~ocaml_version:Ocaml_version.Releases.v4_14_0 ~opam_version:`V2_1
    |> Result.get_ok
  in
  let actual =
    Opam_build.spec
      ~base:
        "ocaml/opam@sha256:03668731d460043acc763d35e1d5dfc6e6fe68a02f987849ac74f855e3e42c10"
      ~opam_version:`V2_1 ~opam_files:[ "bondi.opam" ]
      ~selection:
        Selection.
          {
            variant;
            packages =
              [
                "bondi.dev";
                "base-bigarray.base";
                "base-threads.base";
                "base-unix.base";
                "dune.3.6.0";
                "menhir.20220210";
                "menhirLib.20220210";
                "menhirSdk.20220210";
                "ocaml.4.14.0";
                "ocaml-base-compiler.4.14.0";
                "ocaml-config.2";
                "ocaml-options-vanilla.1";
              ];
            only_packages = [];
            commit = "f207d3f018d642d1fcddb2c118e7fa8e65f4e366";
            lower_bound = false;
          }
    |> Obuilder_spec.sexp_of_t
  in
  Alcotest.(check sexp) "validate" expected actual

let test_windows_spec () =
  let open Ocaml_ci in
  let expected = Sexplib__Pre_sexp.of_string expected_windows_spec in
  let variant =
    Variant.v ~arch:`X86_64 ~distro:"windows-server-2022"
      ~ocaml_version:Ocaml_version.Releases.v4_14_0 ~opam_version:`V2_3
    |> Result.get_ok
  in
  let actual =
    Opam_build.spec ~base:"windows-server-2022-amd64-ocaml-4.14"
      ~opam_version:`V2_3 ~opam_files:[ "bondi.opam" ]
      ~selection:
        Selection.
          {
            variant;
            packages =
              [
                "bondi.dev";
                "arch-x86_64.1";
                "base-bigarray.base";
                "base-threads.base";
                "base-unix.base";
                "conf-mingw-w64-gcc-x86_64.1";
                "dune.3.17.0";
                "flexdll.0.43";
                "host-arch-x86_64.1";
                "host-system-mingw.1";
                "menhir.20240715";
                "menhirCST.20240715";
                "menhirLib.20240715";
                "menhirSdk.20240715";
                "mingw-w64-shims.0.2.0";
                "ocaml.4.14.2";
                "ocaml-base-compiler.4.14.2";
                "ocaml-config.3";
                "ocaml-env-mingw64.1";
                "ocaml-options-vanilla.1";
                "system-mingw.1";
              ];
            only_packages = [];
            commit = "f207d3f018d642d1fcddb2c118e7fa8e65f4e366";
            lower_bound = false;
          }
    |> Obuilder_spec.sexp_of_t
  in
  Alcotest.(check sexp) "validate" expected actual

let tests =
  [
    Alcotest.test_case "macos" `Quick test_macos_spec;
    Alcotest.test_case "linux" `Quick test_linux_spec;
    Alcotest.test_case "windows" `Quick test_windows_spec;
  ]
