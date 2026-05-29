(lang dune 3.20)
(name ocaml-ci)

(generate_opam_files true)
(source (github ocurrent/ocaml-ci))
(authors "talex5@gmail.com")
(maintainers "Mark.Elvers <mark.elvers@tunbury.org>")
(documentation "https://ocaml.ci.dev/documentation")
(license "MIT")

(package
 (name ocaml-ci-api)
 (synopsis "Cap'n Proto API for ocaml-ci")
 (depends
  (ocaml (>= 4.14))
  dune
  current_rpc
  (capnp (>= 3.4.0))
  (capnp-rpc-lwt (>= 1.2))
  (ppx_deriving (>= 5.1))
  (ppx_deriving_yojson (>= 3.7))))

(package
 (name ocaml-ci)
 (synopsis "Core ocaml-ci")
 (depends
  (ocaml (>= 4.14))
  dune
  ocaml-ci-api
  current
  current_docker
  current_ocluster
  current_rpc
  obuilder-spec
  ocluster-api
  solver-service
  solver-service-api
  solver-worker
  (bos (>= 0.2.1))
  (dockerfile-opam (>= 8.3.6))
  (capnp-rpc-unix (>= 1.2))
  (cohttp-lwt-unix (>= 5.1.0))
  (logs (>= 0.7.0))
  (ocaml-version (>= 4.1.0))
  (omigrate (>= 0.3.2))
  (opam-0install (>= 0.4.3))
  (ppx_deriving (>= 5.1))
  (ppx_deriving_yojson (>= 3.7))
  (tar (and (>= 2.4.0) (< 3.0.0)))
  (tar-unix (and (>= 2.4.0) (< 3.0.0)))
  (timedesc (>= 0.9.0))))

(package
 (name ocaml-ci-service)
 (synopsis "Test OCaml projects on GitHub")
 (conflicts (ocaml-migrate-parsetree (= "1.7.1")))
 (depends
  (ocaml (>= 4.14))
  dune
  ocaml-ci-api
  current_git
  current_github
  current_docker
  current_web
  current_rpc
  current_ocluster
  solver-service
  ocluster-api
  obuilder-spec
  (alcotest (and (>= 1.7.0) :with-test))
  (alcotest-lwt (and (>= 1.7.0) :with-test))
  (ansi (>= 0.6.0))
  (astring (>= 0.8.5))
  (bos (>= 0.2.1))
  (capnp-rpc-unix (>= 1.2))
  (cmdliner (>= 1.1.1))
  (conf-libev (<> :os "win32"))
  (dockerfile-opam (>= 8.3.6))
  (fmt (>= 0.8.9))
  (logs (>= 0.7.0))
  (mirage-crypto-rng (>= 0.8.7))
  (ocaml-version (>= 4.1.0))
  (opam-0install (>= 0.4.3))
  (ppx_deriving (>= 5.1))
  (ppx_deriving_yojson (>= 3.7))
  (ppx_sexp_conv (>= v0.14.1))
  (prometheus-app (>= 1.2))
  (timedesc (>= 0.9.0))))

(package
 (name ocaml-ci-client)
 (synopsis "Command-line client for ocaml-ci")
 (depends
  (ocaml (>= 4.14))
  dune
  ocaml-ci-api
  current_rpc
  (capnp-rpc-unix (>= 1.2))
  (dockerfile (>= 8.3.6))
  (fmt (>= 0.8.9))
  (logs (>= 0.7.0))
  (timedesc (>= 0.9.0))))

(package
 (name ocaml-ci-web)
 (synopsis "Web-server frontend for ocaml-ci")
 (depends
  (ocaml (>= 4.14))
  dune
  ocaml-ci-api
  ; Provided by opam pin
  tailwindcss
  (ansi (>= 0.6.0))
  (capnp-rpc-unix (>= 1.2))
  (crunch (and (>= 3.2.0) :build))
  (dream (= 1.0.0~alpha6))
  (dream-httpaf (= 1.0.0~alpha3))
  (fmt (>= 0.8.9))
  (logs (>= 0.7.0))
  (lwt (>= 5.7.0))
  (prometheus-app (>= 1.2))
  (ppx_deriving_yojson (>= 3.7))
  (yojson (>= 2.1))
  (tyxml (>= 4.5))
  (timedesc (>= 0.9.0))))
