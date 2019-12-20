open Current.Syntax
module Docker = Conf.Builder_amd1

let ocamlformat_dockerfile ~base ~ocamlformat_source =
  let open Dockerfile in
  let install_ocamlformat =
    match ocamlformat_source with
    | Ocaml_ci.Analyse_ocamlformat.Vendored { path } ->
      run "opam pin add -yn ocamlformat %S" path
      @@ run "opam depext ocamlformat"
      @@ run "opam install --deps-only -y ocamlformat"
    | Opam { version } ->
      run "opam depext ocamlformat=%s" version
      @@ run "opam install ocamlformat=%s" version
  in
  from (Docker.Image.hash base)
  @@ run "opam install dune" (* Not necessarily the dune version used by the project *)
  @@ workdir "src"
  @@ install_ocamlformat
  @@ copy ~chown:"opam" ~src:["./"] ~dst:"./" ()

let v_fmt ~ocamlformat_source ~base ~src =
  let dockerfile =
    let+ base = base and+ ocamlformat_source = ocamlformat_source in
    ocamlformat_dockerfile ~base ~ocamlformat_source in
  let img =
    Docker.build ~label:"OCamlformat" ~pool:Docker.pool ~pull:false ~dockerfile (`Git src)
  in
  Docker.run ~label:"lint" img ~args:[ "sh"; "-c"; "dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)" ]
