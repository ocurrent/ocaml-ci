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
    run "opam depext -it ocamlformat=%s" version

let fmt_dockerfile ~base ~ocamlformat_source ~for_user =
  let download_cache_prefix = if for_user then "" else Opam_build.download_cache ^ " " in
  let open Dockerfile in
  (if for_user then empty else Buildkit_syntax.add `X86_64) @@
  from base
  @@ run "%sopam install dune" download_cache_prefix (* Not necessarily the dune version used by the project *)
  @@ workdir "src"
  @@ (match ocamlformat_source with
      | Some src -> install_ocamlformat src
      | None -> empty)
  @@ copy ~chown:"opam" ~src:["./"] ~dst:"./" ()
  @@ run "opam exec -- dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)"

let doc_dockerfile ~base ~opam_files ~selection ~for_user =
  let download_cache_prefix = if for_user then "" else Opam_build.download_cache ^ " " in
  let open Dockerfile in
  Opam_build.install_project_deps ~base ~opam_files ~selection ~for_user
  (* Warnings-as-errors was introduced in Odoc.1.5.0 *)
  @@ run "%sopam depext -i dune 'odoc>=1.5.0'" download_cache_prefix
  @@ copy ~chown:"opam" ~src:["."] ~dst:"/src/" ()
  @@ run "ODOC_WARN_ERROR=true opam exec -- dune build @doc \
          || (echo \"dune build @doc failed\"; exit 2)"

let opam_lint_dockerfile ~base ~opam_files ~for_user:_ =
  let open Dockerfile in
  from base
  @@ workdir "src"
  @@ copy ~chown:"opam" ~src:["./"] ~dst:"./" ()
  @@ run "opam lint %s" (String.concat " " opam_files)
