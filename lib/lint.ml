open Current.Syntax

module Make (Docker : S.DOCKER_CONTEXT) = struct

  let install_ocamlformat =
    let open Dockerfile in
    function
    | Analyse_ocamlformat.Vendored { path } ->
      let opam_file = Filename.concat path "ocamlformat.opam" in
      copy ~chown:"opam" ~src:[ opam_file ] ~dst:opam_file ()
      @@ run "opam pin add -k none -yn ocamlformat %S" path
      (* Only the opam file is necessary to install the deps
          [-k none] above ensures that opam doesn't try to make an installable package *)
      @@ run "opam depext ocamlformat"
      @@ run "opam install --deps-only -y ocamlformat"
    | Opam { version } ->
      run "opam depext ocamlformat=%s" version
      @@ run "opam install ocamlformat=%s" version

  let fmt_dockerfile ~base ~ocamlformat_source =
    let open Dockerfile in
    from (Docker.image_hash base)
    @@ run "opam install dune" (* Not necessarily the dune version used by the project *)
    @@ workdir "src"
    @@ (match ocamlformat_source with
        | Some src -> install_ocamlformat src
        | None -> empty)
    @@ copy ~chown:"opam" ~src:["./"] ~dst:"./" ()

  (** An image with formatting dependencies installed and the project ready to be formatted *)
  let fmt_base_image ~base ~ocamlformat_source ~source =
    let dockerfile =
      let+ base = base and+ ocamlformat_source = ocamlformat_source in
      fmt_dockerfile ~base ~ocamlformat_source
    in
    Docker.build ~label:"fmt" ~dockerfile source

  let run_fmt ~img =
    Docker.run ~label:"lint-fmt" img ~args:[ "sh"; "-c"; "dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)" ]

  let v ~schedule ~analysis ~source =
    let base =
      Docker.pull ~schedule "ocurrent/opam:alpine-3.10-ocaml-4.08"
    in
    analysis
    |> Current.map Analyse.Analysis.ocamlformat_source
    |> (fun ocamlformat_source ->
        let img = fmt_base_image ~base ~ocamlformat_source ~source in
        run_fmt ~img
      )
    |> Current.map (fun () -> `Checked)

end
