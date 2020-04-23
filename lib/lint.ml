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

let fmt_dockerfile ~base ~info ~variant ~for_user =
  ignore (variant, for_user); (* (todo) *)
  let ocamlformat_source = Analyse.Analysis.ocamlformat_source info in
  let open Dockerfile in
  from base
  @@ run "opam install dune" (* Not necessarily the dune version used by the project *)
  @@ workdir "src"
  @@ (match ocamlformat_source with
      | Some src -> install_ocamlformat src
      | None -> empty)
  @@ copy ~chown:"opam" ~src:["./"] ~dst:"./" ()
  @@ run "opam exec -- dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)"

let doc_dockerfile ~base:_ ~info:_ ~variant:_ ~for_user:_ =
  let open Dockerfile in
  (*  Opam_build.install_project_deps ~base ~info ~variant ~for_user *)
  assert false
  @@ run "opam depext odoc"
  (* Warnings-as-errors was introduced in Odoc.1.5.0 *)
  @@ run "opam install dune odoc>=1.5.0"
  @@ run "ODOC_WARN_ERROR=true opam exec -- dune build @doc \
          || (echo \"dune build @doc failed\"; exit 2)"
