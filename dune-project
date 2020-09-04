(lang dune 2.0)
(name ocaml-ci-api)
(formatting disabled)

(generate_opam_files true)

(source (github ocurrent/ocaml-ci))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")

(package
 (name ocaml-ci-api)
 (synopsis "Cap'n Proto API for ocaml-ci")
 (depends
  current_rpc
  (capnp (>= 3.4.0))
  capnp-rpc-lwt
  dockerfile
  ppx_deriving
  ppx_deriving_yojson))

(package
 (name ocaml-ci-service)
 (synopsis "Test OCaml projects on GitHub")
 (conflicts (ocaml-migrate-parsetree (= "1.7.1")))
 (depends
  current_git
  current_github
  current_docker
  current_web
  current_rpc
  (capnp-rpc-unix (>= 0.7.0))
  ocaml-ci-api
  ocaml-ci-solver
  ocluster-api
  conf-libev
  (dockerfile (>= 7.0.0))
  (dockerfile-opam (>= 7.0.0))
  (ocaml-version (>= 3.0.0))
  (alcotest (and (>= 1.0.0) :with-test))
  (alcotest-lwt (and (>= 1.0.1) :with-test))
))

(package
 (name ocaml-ci-solver)
 (synopsis "Choose package versions to test")
 (depends
  yojson
  lwt
  logs
  fmt
  ocaml-ci-api
  conf-libev
  (opam-0install (>= "0.2"))
  git-unix
  (capnp-rpc-unix (>= 0.7.0))
))

(package
 (name ocaml-ci-web)
 (synopsis "Web-server frontend for ocaml-ci")
 (conflicts (ocaml-migrate-parsetree (= "1.7.1")))
 (depends
  current_rpc
  current_ansi
  prometheus-app
  cmdliner
  lwt
  (cohttp-lwt-unix (>= 2.2.0))
  tyxml
  capnp-rpc-unix
  ocaml-ci-api
  conf-libev
  dockerfile
  ppx_deriving_yojson
))

(package
 (name ocaml-ci-client)
 (synopsis "Command-line client for ocaml-ci")
 (depends
  current_rpc
  capnp-rpc-unix
  ocaml-ci-api
  dockerfile
))
