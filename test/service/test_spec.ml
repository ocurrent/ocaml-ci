let macos_distro = "macos-homebrew"

(* Expected obuilder spec for macos build of bondi. *)
let expected_macos_spec =
  Printf.sprintf
    {|
((from macos-homebrew-ocaml-4.14)
 (comment macos-homebrew-4.14.0_opam-2.1)
 (user (uid 1000) (gid 1000)) (env CLICOLOR_FORCE 1)
 (env OPAMCOLOR always)
 (run (shell "ln -f ~/local/bin/opam-2.1 ~/local/bin/opam"))
 (run (shell "opam init --reinit -ni"))
 (run (cache (opam-archives (target ~/.opam/download-cache)))
      (network host)
      (shell "cd ~/opam-repository && (git cat-file -e f207d3f018d642d1fcddb2c118e7fa8e65f4e366 || git fetch origin master) && git reset -q --hard f207d3f018d642d1fcddb2c118e7fa8e65f4e366 && git log --no-decorate -n1 --oneline && opam update -u"))
 (copy (src bondi.opam) (dst ./src/./))
 (run (network host)
      (shell "opam pin add -yn bondi.dev './src/./'"))
 (env DEPS "base-bigarray.base base-threads.base base-unix.base dune.3.6.0 menhir.20220210 menhirLib.20220210 menhirSdk.20220210 ocaml.4.14.0 ocaml-base-compiler.4.14.0 ocaml-config.2 ocaml-options-vanilla.1")
 (env CI true)
 (env OCAMLCI true)
 (run (cache (opam-archives (target ~/.opam/download-cache)))
      (network host)
      (shell "opam update --depexts && opam install --cli=2.1 --depext-only -y bondi.dev $DEPS"))
 (run (cache (opam-archives (target ~/.opam/download-cache)))
      (network host)
      (shell "opam install $DEPS"))
 (copy (src .) (dst ./src))
 (run (shell "cd ./src && opam exec -- dune build --only-packages=bondi @install @check @runtest;
res=$?;
test \"$res\" != 1 && exit \"$res\";
ret=1;
for pkg in bondi; do
  if opam show -f x-ci-accept-failures: \"$pkg\" | grep -qF \"\\\"%s\\\"\"; then
    echo -e \"The package \\033[1m$pkg\\033[0m failed, but has been disabled for CI on \\033[1m%s\\033[0m using the \\033[1mx-ci-accept-failures\\033[0m field in its opam file.\";
    ret=0;
  fi;
done;
exit \"$ret\";
&& rm -rf _build"))
)|}
    macos_distro macos_distro

let linux_distro = "debian-11"

(* Expected obuilder spec for linux (debian-11) build of bondi. *)
let expected_linux_spec =
  Printf.sprintf
    {|
((from ocaml/opam@sha256:03668731d460043acc763d35e1d5dfc6e6fe68a02f987849ac74f855e3e42c10)
 (comment debian-11-4.14.0_opam-2.1)
 (user (uid 1000) (gid 1000)) (env CLICOLOR_FORCE 1)
 (env OPAMCOLOR always)
 (workdir /src)
 (run (shell "sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam"))
 (run (shell "opam init --reinit -ni"))
 (workdir /src)
 (run (shell "sudo chown opam /src"))
 (run (cache (opam-archives (target /home/opam/.opam/download-cache)))
      (network host)
      (shell "cd ~/opam-repository && (git cat-file -e f207d3f018d642d1fcddb2c118e7fa8e65f4e366 || git fetch origin master) && git reset -q --hard f207d3f018d642d1fcddb2c118e7fa8e65f4e366 && git log --no-decorate -n1 --oneline && opam update -u"))
 (copy (src bondi.opam) (dst ./))
 (run (network host)
      (shell "opam pin add -yn bondi.dev './'"))
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
 (run (shell "opam exec -- dune build --only-packages=bondi @install @check @runtest;
res=$?;
test \"$res\" != 1 && exit \"$res\";
ret=1;
for pkg in bondi; do
  if opam show -f x-ci-accept-failures: \"$pkg\" | grep -qF \"\\\"%s\\\"\"; then
    echo -e \"The package \\033[1m$pkg\\033[0m failed, but has been disabled for CI on \\033[1m%s\\033[0m using the \\033[1mx-ci-accept-failures\\033[0m field in its opam file.\";
    ret=0;
  fi;
done;
exit \"$ret\";
&& rm -rf _build"))
)|}
    linux_distro linux_distro

(* Create testable Sexp for Alcotest. *)
let sexp = Alcotest.testable Sexplib0__Sexp.pp_hum Sexplib0__Sexp.equal

let test_macos_spec () =
  let open Ocaml_ci in
  let expected = Sexplib__Pre_sexp.of_string expected_macos_spec in
  let variant =
    Variant.v ~arch:`X86_64 ~distro:macos_distro
      ~ocaml_version:Ocaml_version.Releases.v4_14_0 ~opam_version:`V2_1
    |> Result.get_ok
  in
  let actual =
    Opam_build.spec
      ~base:(macos_distro ^ "-ocaml-4.14")
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
            compatible_root_pkgs = [ "bondi.dev" ];
            commit = "f207d3f018d642d1fcddb2c118e7fa8e65f4e366";
          }
    |> Obuilder_spec.sexp_of_t
  in
  Alcotest.(check sexp) "validate" expected actual

let test_linux_spec () =
  let open Ocaml_ci in
  let expected = Sexplib__Pre_sexp.of_string expected_linux_spec in
  let variant =
    Variant.v ~arch:`X86_64 ~distro:linux_distro
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
            compatible_root_pkgs = [ "bondi.dev" ];
            commit = "f207d3f018d642d1fcddb2c118e7fa8e65f4e366";
          }
    |> Obuilder_spec.sexp_of_t
  in
  Alcotest.(check sexp) "validate" expected actual

let tests =
  [
    Alcotest.test_case "macos" `Quick test_macos_spec;
    Alcotest.test_case "linux" `Quick test_linux_spec;
  ]
