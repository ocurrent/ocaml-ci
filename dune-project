(lang dune 3.7)
(name ocaml-ci)

(generate_opam_files true)
(source (github ocurrent/ocaml-ci))
(authors "talex5@gmail.com")
(maintainers "Navin Keswani <navin@novemberkilo.com>" "Tim McGilchrist <timmcgil@gmail.com>")
(documentation "https://ocaml.ci.dev/documentation")

(package
 (name ocaml-ci-api)
 (synopsis "Cap'n Proto API for ocaml-ci")
 (depends
  (ocaml (>= 4.14))
  ; Provided by git submodules
  current_rpc
  ; Opam dependencies
  (capnp (>= 3.4.0))
  (capnp-rpc-lwt (>= 1.2))
  (ppx_deriving (>= 5.1))
  (ppx_deriving_yojson (>= 3.7))))

(package
 (name ocaml-ci)
 (synopsis "Core ocaml-ci")
 (depends
  (ocaml (>= 4.14))
  ocaml-ci-api
  ; Provided by git submodules
  current
  current_docker
  current_ocluster
  current_rpc
  obuilder-spec
  ocluster-api
  solver-service
  ; Opam dependencies
  (bos (>= 0.2.1))
  (capnp-rpc-unix (>= 1.2))
  (cohttp-lwt-unix (>= 5.1.0))
  (logs (>= 0.7.0))
  (ocaml-version (>= 3.6.1))
  (omigrate (>= 0.3.2))
  (opam-0install (>= 0.4.3))
  (ppx_deriving (>= 5.1))
  (ppx_deriving_yojson (>= 3.7))
  (tar (>= 2.4.0))
  (tar-unix (>= 2.4.0))
  (timedesc (>= 0.9.0))))

(package
 (name ocaml-ci-service)
 (synopsis "Test OCaml projects on GitHub")
 (conflicts (ocaml-migrate-parsetree (= "1.7.1")))
 (depends
  (ocaml (>= 4.14))
  ocaml-ci-api
  ; Provided by git submodules
  current_git
  current_github
  current_docker
  current_web
  current_rpc
  current_ocluster
  solver-service
  ocluster-api
  obuilder-spec
  ; Opam dependencies
  (alcotest (and (>= 1.7.0) :with-test))
  (alcotest-lwt (and (>= 1.7.0) :with-test))
  (ansi (>= 0.6.0))
  (astring (>= 0.8.5))
  (bos (>= 0.2.1))
  (capnp-rpc-unix (>= 1.2))
  (cmdliner (>= 1.1.1))
  (conf-libev (<> :os "win32"))
  (dockerfile-opam (>= 8.2.1))
  (fmt (>= 0.8.9))
  (logs (>= 0.7.0))
  (mirage-crypto-rng (>= 0.8.7))
  (ocaml-version (>= 3.6.1))
  (opam-0install (>= 0.4.3))
  (ppx_deriving (>= 5.1))
  (ppx_deriving_yojson (>= 3.7))
  (ppx_sexp_conv (>= v0.14.1))
  (prometheus-app (>= 1.2))
  (timedesc (>= 0.9.0))))

(package
 (name ocaml-ci-gitlab)
 (synopsis "Test OCaml projects on GitLab")
 (description "Test OCaml projects on GitLab")
 (depends
  (ocaml (>= 4.14))
  ocaml-ci
  ocaml-ci-service
  ; Provided by git submodules
  current
  current_git
  current_gitlab
  current_docker
  ; Opam dependencies
  (alcotest (and (>= 1.7.0) :with-test))
  (cmdliner (>= 1.1.1))
  (fmt (>= 0.8.9))
  (gitlab-unix (>= 0.1.8))
  (logs (>= 0.7.0))
  (odoc :with-doc)
  (prometheus-app (>= 1.2))))

(package
 (name ocaml-ci-client)
 (synopsis "Command-line client for ocaml-ci")
 (depends
  (ocaml (>= 4.14))
  ocaml-ci-api
  ; Provided by git submodules
  current_rpc
  ; Opam dependencies
  (capnp-rpc-unix (>= 1.2))
  (dockerfile (>= 8.2.1))
  (fmt (>= 0.8.9))
  (logs (>= 0.7.0))
  (timedesc (>= 0.9.0))))

(package
 (name ocaml-ci-web)
 (synopsis "Web-server frontend for ocaml-ci")
 (depends
  (ocaml (>= 4.14))
  ocaml-ci-api
  ; Provided by opam pin
  tailwindcss
  ; Opam dependencies
  (ansi (>= 0.6.0))
  (capnp-rpc-unix (>= 1.2))
  (crunch (and (>= 3.2.0) :build))
  (dream (= 1.0.0~alpha5))
  (fmt (>= 0.8.9))
  (logs (>= 0.7.0))
  (lwt (>= 5.6.1))
  (prometheus-app (>= 1.2))
  (ppx_deriving_yojson (>= 3.7))
  (yojson (>= 2.1))
  (tyxml (>= 4.5))
  (timedesc (>= 0.9.0))))
