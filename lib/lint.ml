open Current.Syntax

module Docker = Current_docker.Raw

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
  from (Docker.Image.hash base)
  @@ run "opam install dune" (* Not necessarily the dune version used by the project *)
  @@ workdir "src"
  @@ (match ocamlformat_source with
      | Some src -> install_ocamlformat src
      | None -> empty)
  @@ copy ~chown:"opam" ~src:["./"] ~dst:"./" ()

(** An image with formatting dependencies installed and the project ready to be formatted *)
let fmt_base_image ~builder ~base ~ocamlformat_source ~source =
  Current.component "build-fmt-tools" |>
  let> base = base
  and> ocamlformat_source = ocamlformat_source
  and> source = source
  in
  let dockerfile = `Contents (fmt_dockerfile ~base ~ocamlformat_source) in
  Builder.build builder source ~dockerfile

let run_fmt ~builder ~img =
  Current.component "lint-fmt" |>
  let> img = img in
  Builder.run builder img
    ~args:[ "sh"; "-c"; "dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)" ]

let pull ~builder ~schedule tag =
  Current.component "pull %s" tag |>
  let> () = Current.return () in
  Builder.pull builder tag ~schedule

let v ~builder ~schedule ~analysis ~source =
  let base = pull ~builder ~schedule "ocurrent/opam:alpine-3.10-ocaml-4.08" in
  analysis
  |> Current.map Analyse.Analysis.ocamlformat_source
  |> (fun ocamlformat_source ->
      let img = fmt_base_image ~builder ~base ~ocamlformat_source ~source in
      run_fmt ~builder ~img
    )
  |> Current.map (fun () -> `Checked)
