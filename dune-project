(lang dune 1.11)
(name ocaml-ci)
(using fmt 1.0)

(generate_opam_files true)

(source (github ocurrent/ocaml-ci))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")

(package
 (name ocaml-ci-api)
 (synopsis "Cap'n Proto API for ocaml-ci")
 (depends
  (dune (>= "1.11"))
  current_rpc
  (capnp (>= 3.4.0))
  capnp-rpc-lwt
  dockerfile))

(package
 (name ocaml-ci-service)
 (synopsis "Test OCaml projects on GitHub")
 (depends
  (dune (>= "1.11"))
  current_git
  current_github
  current_docker
  current_web
  current_rpc
  (capnp-rpc-unix (>= 0.5.0))
  ocaml-ci-api
  conf-libev
  (dockerfile (>= 6.3.0))
  (alcotest (and (>= 1.0.0) :with-test))
  (alcotest-lwt (and (>= 1.0.1) :with-test))
))

(package
 (name ocaml-ci-web)
 (synopsis "Web-server frontend for ocaml-ci")
 (depends
  (dune (>= "1.11"))
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
  (dune (>= "1.11"))
  current_rpc
  capnp-rpc-unix
  ocaml-ci-api
  dockerfile
))
