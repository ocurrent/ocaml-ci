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
  ppx_deriving
  ppx_deriving_yojson))
(package
 (name ocaml-ci-service)
 (synopsis "Test OCaml projects on GitHub")
 (conflicts (ocaml-migrate-parsetree (= "1.7.1")))
 (depends
  (prometheus-app (>= 1.0))
  (ppx_sexp_conv (>= v0.14.1))
  (ppx_deriving_yojson (>= 3.6.1))
  (ppx_deriving (>= 5.1))
  (opam-0install (>= 0.4))
  (mirage-crypto-rng (>= 0.8.7))
  (logs (>= 0.7.0))
  (fmt (>= 0.8.9))
  (cmdliner (>= 1.1.0))
  current_git
  current_github
  current_docker
  current_web
  current_rpc
  current_ocluster
  (capnp-rpc-unix (>= 1.2))
  ocaml-ci-api
  ocaml-ci-solver
  ocluster-api
  obuilder-spec
  conf-libev
  (dockerfile-opam (>= 7.0.0))
  (ocaml-version (>= 3.0.0))
  matrix-current
  (alcotest (and (>= 1.0.0) :with-test))
  (alcotest-lwt (and (>= 1.0.1) :with-test))))
(package
 (name ocaml-ci-solver)
 (synopsis "Choose package versions to test")
 (depends
  (ppx_deriving_yojson (>= 3.6.1))
  (ppx_deriving (>= 5.1))
  yojson
  lwt
  logs
  fmt
  ocaml-ci-api
  conf-libev
  (opam-0install (>= "0.2"))
  (git-unix (>= 3.8.0))
  (capnp-rpc-unix (>= 1.2)))
 (conflicts
  (carton (< 0.4.2))) ; workaround for mirage/ocaml-git#514
)
(package
 (name ocaml-ci-web)
 (synopsis "Web-server frontend for ocaml-ci")
 (conflicts (ocaml-migrate-parsetree (= "1.7.1")))
 (depends
  (yojson (>= 1.7.0))
  (logs (>= 0.7.0))
  (fmt (>= 0.8.9))
  current_rpc
  current_ansi
  (prometheus-app (>= 1.0))
  (cmdliner (>= 1.1.0))
  lwt
  (cohttp-lwt-unix (>= 2.2.0))
  tyxml
  (capnp-rpc-unix (>= 1.2))
  ocaml-ci-api
  conf-libev
  ppx_deriving_yojson))
(package
 (name ocaml-ci-client)
 (synopsis "Command-line client for ocaml-ci")
 (depends
  (logs (>= 0.7.0))
  (fmt (>= 0.8.9))
  current_rpc
  (capnp-rpc-unix (>= 1.2))
  ocaml-ci-api
  dockerfile))
