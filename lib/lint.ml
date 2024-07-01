(* Use opam-2.1 to lint the packages *)
let opam_version = `V2_1

let install_ocamlformat =
  let open Obuilder_spec in
  let cache =
    [
      Obuilder_spec.Cache.v Opam_build.download_cache
        ~target:"/home/opam/.opam/download-cache";
    ]
  in
  let network = [ "host" ] in
  function
  | Analyse_ocamlformat.Vendored { path } ->
      let opam_file = Filename.concat path "ocamlformat.opam" in
      [
        copy [ opam_file ] ~dst:opam_file;
        run ~network "opam pin add -k path -n ocamlformat %S" path;
        (* Pinned to a directory containing only the .opam file *)
        run ~network "opam depext ocamlformat";
        run ~network ~cache "opam install --deps-only -y ocamlformat";
      ]
  | Opam { version; _ } ->
      [ run ~network ~cache "opam depext -i ocamlformat=%s" version ]

let commit_from_ocamlformat_source ocamlformat_source =
  let open Analyse_ocamlformat in
  match ocamlformat_source with
  | Vendored _ -> None
  | Opam { opam_repo_commit; _ } -> opam_repo_commit

let fmt_spec ~base ~ocamlformat_source ~selection =
  let open Obuilder_spec in
  let {
    Selection.packages = _;
    commit;
    variant = _;
    only_packages = _;
    lower_bound = _;
  } =
    selection
  in
  let cache =
    [
      Obuilder_spec.Cache.v Opam_build.download_cache
        ~target:"/home/opam/.opam/download-cache";
    ]
  in
  let actions =
    match ocamlformat_source with
    | Error (`Msg msg) ->
        [ user_unix ~uid:1000 ~gid:1000; run "echo Error: %s; exit 2" msg ]
    | Ok None ->
        (* TODO This is a workaround for https://github.com/ocaml/dune/issues/10578
           Once [dune build @fmt] allows clean exits even when ocamlformat is not installed,
           we should remove this case, so that we still have the benefit of running other
           formatting lints that may be associated with the @fmt alias. *)
        [
          run
            {| echo "skipping format lint because ocamlformat is not configured" |};
        ]
    | Ok (Some source) ->
        let commit =
          Option.value ~default:commit (commit_from_ocamlformat_source source)
        in
        let network = [ "host" ] in
        [
          user_unix ~uid:1000 ~gid:1000;
          run ~network ~cache
            "cd ~/opam-repository && (git cat-file -e %s || git fetch origin \
             master) && git reset -q --hard %s && git log --no-decorate -n1 \
             --oneline && opam update -u"
            commit commit;
          run ~network ~cache "opam depext -i dune";
          (* Necessary in case current compiler < 4.08 *)
          (* Not necessarily the dune version used by the project *)
          workdir "/src";
        ]
        @ install_ocamlformat source
        @ [
            copy [ "." ] ~dst:"/src/";
            run
              "opam exec -- dune build @fmt --ignore-promoted-rules || (echo \
               \"dune build @fmt failed\"; exit 2)";
          ]
  in
  stage ~from:base actions

let doc_spec ~base ~opam_files ~selection =
  let cache =
    [
      Obuilder_spec.Cache.v Opam_build.download_cache
        ~target:"/home/opam/.opam/download-cache";
    ]
  in
  let network = [ "host" ] in
  let open Obuilder_spec in
  let to_name x = OpamPackage.of_string x |> OpamPackage.name_to_string in
  let only_packages =
    match selection.Selection.only_packages with
    | [] -> ""
    | pkgs -> " --only-packages=" ^ String.concat "," (List.map to_name pkgs)
  in
  stage ~from:base
  @@ comment "%s" (Fmt.str "%a" Variant.pp selection.Selection.variant)
     :: user_unix ~uid:1000 ~gid:1000
     :: Opam_build.install_project_deps ~opam_version ~opam_files ~selection
  @ [
      run ~network ~cache "opam install --yes dune 'odoc>=1.5.0'";
      copy [ "." ] ~dst:"/src/";
      (* Warnings-as-errors was introduced in Odoc.1.5.0 *)
      run
        "ODOC_WARN_ERROR=false opam exec -- dune build%s @doc || (echo \"dune \
         build @doc failed\"; exit 2)"
        only_packages;
    ]

let opam_dune_lint_spec ~base ~opam_files ~selection =
  let open Obuilder_spec in
  stage ~from:base
  @@ comment "%s" (Fmt.str "%a" Variant.pp selection.Selection.variant)
     :: user_unix ~uid:1000 ~gid:1000
     :: Opam_build.install_project_deps ~opam_version ~opam_files ~selection
  @ [
      env "CI" "true";
      env "OCAMLCI" "true";
      workdir "/src";
      copy [ "." ] ~dst:"/src/";
      run "opam lint";
      run "opam exec -- opam-dune-lint";
    ]

let opam_lint_spec ~base ~opam_files =
  let open Obuilder_spec in
  stage ~from:base
    [
      user_unix ~uid:1000 ~gid:1000;
      workdir "src";
      copy [ "./" ] ~dst:"./";
      run "opam lint %s" (String.concat " " opam_files);
    ]
