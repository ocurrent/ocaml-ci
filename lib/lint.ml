let install_ocamlformat =
  let open Obuilder_spec in
  let cache = [ Obuilder_spec.Cache.v Opam_build.download_cache ~target:"/home/opam/.opam/download-cache" ] in
  let network = ["host"] in
  function
  | Analyse_ocamlformat.Vendored { path } ->
    let opam_file = Filename.concat path "ocamlformat.opam" in
    [
      copy [ opam_file ] ~dst:opam_file;
      run "opam pin add -k path -n ocamlformat %S" path;
      (* Pinned to a directory containing only the .opam file *)
      run ~network "opam depext ocamlformat";
      run ~network ~cache "opam install --deps-only -y ocamlformat";
    ]
  | Opam { version } ->
    [ run ~network ~cache "opam depext -it ocamlformat=%s" version ]

let fmt_spec ~base ~ocamlformat_source =
  let open Obuilder_spec in
  let cache = [ Obuilder_spec.Cache.v Opam_build.download_cache ~target:"/home/opam/.opam/download-cache" ] in
  let network = ["host"] in
  stage ~from:base @@ [
    user ~uid:1000 ~gid:1000;
    run ~network ~cache "opam depext -i dune";  (* Necessary in case current compiler < 4.08 *)
                                                (* Not necessarily the dune version used by the project *)
    workdir "src";
  ] @ (match ocamlformat_source with
      | Some src -> install_ocamlformat src
      | None -> []) @ [
    copy ["./"] ~dst:"./";
    run "opam exec -- dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)";
  ]

let doc_spec ~base ~opam_files ~selection =
  let cache = [ Obuilder_spec.Cache.v Opam_build.download_cache ~target:"/home/opam/.opam/download-cache" ] in
  let network = ["host"] in
  let open Obuilder_spec in
  stage ~from:base @@
    user ~uid:1000 ~gid:1000 ::
    Opam_build.install_project_deps ~opam_files ~selection @ [
      (* Warnings-as-errors was introduced in Odoc.1.5.0 *)
      (* conf-m4 is a work-around for https://github.com/ocaml-opam/opam-depext/pull/132 *)
      run ~network ~cache "opam depext -i conf-m4 && opam depext -i dune 'odoc>=1.5.0'";
      copy ["."] ~dst:"/src/";
      run "ODOC_WARN_ERROR=true opam exec -- dune build @doc \
           || (echo \"dune build @doc failed\"; exit 2)";
    ]

let opam_lint_spec ~base ~opam_files =
  let open Obuilder_spec in
  stage ~from:base [
    user ~uid:1000 ~gid:1000;
    workdir "src";
    copy ["./"] ~dst:"./";
    run "opam lint %s" (String.concat " " opam_files);
  ]
