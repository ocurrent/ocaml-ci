(* If the package's directory name doesn't contain a dot then opam will default to
   using the last known version, which is usually wrong. In particular, if a multi-project
   repostory adds a new package with a constraint "{ =version }" on an existing one,
   this will fail because opam will pin the new package as "dev" but the old one with
   the version of its last release. *)
let maybe_add_dev ~dir name =
  if Fpath.is_current_dir dir || not (String.contains (Fpath.basename dir) '.') then name ^ ".dev" else name

(* Group opam files by directory.
   e.g. ["a/a1.opam"; "a/a2.opam"; "b/b1.opam"] ->
        [("a", ["a/a1.opam"; "a/a2.opam"], ["a1.dev"; "a2.dev"]);
         ("b", ["b/b1.opam"], ["b1.dev"])
        ] *)
let group_opam_files =
  ListLabels.fold_left ~init:[] ~f:(fun acc x ->
      let item = Fpath.v x in
      let dir = Fpath.parent item in
      let pkg = Filename.basename x |> Filename.chop_extension |> maybe_add_dev ~dir in
      match acc with
      | (prev_dir, prev_items, pkgs) :: rest when Fpath.equal dir prev_dir -> (prev_dir, x :: prev_items, pkg :: pkgs) :: rest
      | _ -> (dir, [x], [pkg]) :: acc
    )

(* Generate instructions to copy all the files in [items] into the
   image, creating the necessary directories first, and then pin them all. *)
let pin_opam_files ~network groups =
  let open Obuilder_spec in
  let dirs =
    groups
    |> List.map (fun (dir, _, _) -> Filename.quote (Fpath.to_string dir))
    |> String.concat " "
  in
  run "mkdir -p %s" dirs :: (
    groups |> List.map (fun (dir, files, _) ->
        copy files ~dst:(Fpath.to_string dir)
      )
  ) @ [
    groups |> List.concat_map (fun (dir, _, pkgs) ->
        pkgs
        |> List.map (fun pkg ->
            Printf.sprintf "opam pin add -yn %s %s" pkg (Filename.quote (Fpath.to_string dir))
          )
      )
    |> String.concat " && \n"
    |> run ~network "%s"
  ]

(* Get the packages directly in "." *)
let rec get_root_opam_packages = function
  | [] -> []
  | (dir, _, pkgs) ::_ when Fpath.is_current_dir dir -> pkgs
  | _ :: rest -> get_root_opam_packages rest

let download_cache = "opam-archives"

let install_project_deps ~opam_files ~selection =
  let { Selection.packages; commit; variant } = selection in
  let groups = group_opam_files opam_files in
  let root_pkgs = get_root_opam_packages groups in
  let non_root_pkgs = packages |> List.filter (fun pkg -> not (List.mem pkg root_pkgs)) in
  let open Obuilder_spec in
  let cache = [ Obuilder_spec.Cache.v download_cache ~target:"/home/opam/.opam/download-cache" ] in
  let network = ["host"] in
  let distro_extras =
    if Astring.String.is_prefix ~affix:"fedora" (Variant.id variant) then
      [run ~network "sudo dnf install -y findutils"] (* (we need xargs) *)
    else
      []
  in
  let network = ["host"] in
  (if Variant.arch variant |> Ocaml_version.arch_is_32bit then
     [shell ["/usr/bin/linux32"; "/bin/sh"; "-c"]] else []) @ [
    comment "%s" (Fmt.strf "%a" Variant.pp variant);
  ] @ distro_extras @ [
    workdir "/src";
    run "sudo chown opam /src";
    run ~network ~cache
      "cd ~/opam-repository && \
       (git cat-file -e %s || git fetch origin master) && \
       git reset -q --hard %s && git log --no-decorate -n1 --oneline \
       && opam update -u" commit commit;
  ] @ pin_opam_files ~network groups @ [
    env "DEPS" (String.concat " " non_root_pkgs);
    run ~network ~cache "opam depext --update -y %s $DEPS" (String.concat " " root_pkgs);
    run ~network ~cache "opam install $DEPS"
  ]

let is_latest_ocaml v =
  Ocaml_version.equal Ocaml_version.Releases.latest v

let get_dune_option selection =
  if is_latest_ocaml (Variant.ocaml_version selection.Selection.variant) then
    "" (* default options *)
  else
    " --profile=release" (* Disable warnings-as-errors on older compilers *)

let spec ~base ~opam_files ~selection =
  let open Obuilder_spec in
  let option = get_dune_option selection in
  stage ~from:base (
    user ~uid:1000 ~gid:1000 ::
    install_project_deps ~opam_files ~selection @ [
      copy ["."] ~dst:"/src/";
      run "opam exec -- dune build%s @install @runtest && rm -rf _build" option
    ]
  )
