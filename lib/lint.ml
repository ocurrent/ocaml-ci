open Current.Syntax

module Make (Docker : S.DOCKER_CONTEXT) = struct

  let ocamlformat_dockerfile ~base ~ocamlformat_source =
    let open Dockerfile in
    let install_ocamlformat =
      match ocamlformat_source with
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
    in
    from (Docker.image_hash base)
    @@ run "opam install dune" (* Not necessarily the dune version used by the project *)
    @@ workdir "src"
    @@ install_ocamlformat
    @@ copy ~chown:"opam" ~src:["./"] ~dst:"./" ()

  (** An image with OCamlformat installed and the project ready to be formatted *)
  let ocamlformat_base_image ~base ~ocamlformat_source ~source =
    let dockerfile =
      let+ base = base and+ ocamlformat_source = ocamlformat_source in
      ocamlformat_dockerfile ~base ~ocamlformat_source
    in
    Docker.build ~label:"OCamlformat" ~dockerfile source

  let run_fmt ~img =
    Docker.run ~label:"lint" img ~args:[ "sh"; "-c"; "dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)" ]

  let v ~analysis ~source =
    let base = Docker.pull "ocurrent/opam:alpine-3.10-ocaml-4.08" in
    analysis
    |> Current.map Analyse.Analysis.ocamlformat_source
    |> Current.option_map (fun ocamlformat_source ->
        let img = ocamlformat_base_image ~base ~ocamlformat_source ~source in
        run_fmt ~img
      )
    |> Current.map (function
        | Some () -> `Checked
        | None -> `Check_skipped
      )

end
