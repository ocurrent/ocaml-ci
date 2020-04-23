let install_ocamlformat =
  let open Dockerfile in
  function
  | Analyse_ocamlformat.Vendored { path } ->
    let opam_file = Filename.concat path "ocamlformat.opam" in
    copy ~chown:"opam" ~src:[ opam_file ] ~dst:opam_file ()
    @@ run "opam pin add -k path -n ocamlformat %S" path
    (* Pinned to a directory containing only the .opam file *)
    @@ run "opam depext ocamlformat"
    @@ run "opam install --deps-only -y ocamlformat"
  | Opam { version } ->
    run "opam depext ocamlformat=%s" version
    @@ run "opam install ocamlformat=%s" version

let fmt_dockerfile ~base ~ocamlformat_source =
  let open Dockerfile in
  from base
  @@ run "opam install dune" (* Not necessarily the dune version used by the project *)
  @@ workdir "src"
  @@ (match ocamlformat_source with
      | Some src -> install_ocamlformat src
      | None -> empty)
  @@ copy ~chown:"opam" ~src:["./"] ~dst:"./" ()
  @@ run "opam exec -- dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)"

let dockerfile ~base ~info ~variant ~for_user =
  ignore (variant, for_user); (* (todo) *)
  let ocamlformat_source = Analyse.Analysis.ocamlformat_source info in
  fmt_dockerfile ~base ~ocamlformat_source
