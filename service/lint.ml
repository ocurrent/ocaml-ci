open Current.Syntax
module Docker = Conf.Builder_amd1

type ocamlformat_version = [
  | `Vendored of string
  | `Version of string
]

let ocamlformat ~ocamlformat_version ~base ~src =
  let dockerfile =
    let open Dockerfile in
    let+ base = base
    and+ install_ocamlformat =
      let+ ocamlformat_version = ocamlformat_version in
      match ocamlformat_version with
      | `Vendored path ->
        run "opam pin add -yn ocamlformat %S" path
        @@ run "opam depext ocamlformat"
        @@ run "opam install --deps-only -y ocamlformat"
      | `Version v ->
        run "opam depext ocamlformat=%s" v
        @@ run "opam install ocamlformat=%s" v
    in
    from (Docker.Image.hash base)
    @@ run "opam install dune" (* Not the dune version the project use *)
    @@ workdir "src"
    @@ install_ocamlformat
    @@ copy ~chown:"opam" ~src:["./"] ~dst:"./src" ()
  in
  let img =
    Docker.build ~label:"OCamlformat" ~pool:Docker.pool ~pull:false ~dockerfile (`Git src)
  in
  Docker.run ~label:"lint" img ~args:[ "sh"; "-c"; "dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)" ]
