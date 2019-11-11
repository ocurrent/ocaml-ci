open Current.Syntax
module Docker = Conf.Builder_amd1

let format_dockerfile ~base ~ocamlformat_version =
  let open Dockerfile in
  from (Docker.Image.hash base)
  @@ run "opam depext ocamlformat=%s" ocamlformat_version
  @@ run "opam install ocamlformat=%s" ocamlformat_version
  @@ copy ~chown:"opam" ~src:["./"] ~dst:"./src" ()
  @@ workdir "src"

let v_from_opam ~ocamlformat_version ~base ~src =
  let dockerfile =
    let+ base = base and+ ocamlformat_version = ocamlformat_version in
    format_dockerfile ~base ~ocamlformat_version
  in
  let img =
    Docker.build ~label:"OCamlformat" ~pool:Docker.pool ~pull:false ~dockerfile (`Git src)
  in
  Docker.run ~label:"lint" img ~args:[ "sh"; "-c"; "dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)" ]
