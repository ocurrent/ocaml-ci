open Ocaml_ci
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

  let v_fmt ~ocamlformat_source ~base ~src =
    let dockerfile =
      let+ base = base and+ ocamlformat_source = ocamlformat_source in
      ocamlformat_dockerfile ~base ~ocamlformat_source in
    let img = Docker.build ~label:"OCamlformat" ~dockerfile (`Git src) in
    Docker.run ~label:"lint" img ~args:[ "sh"; "-c"; "dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)" ]

end
