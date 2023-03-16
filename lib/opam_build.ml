(* If the package's directory name doesn't contain a dot then opam will default to
   using the last known version, which is usually wrong. In particular, if a multi-project
   repostory adds a new package with a constraint "{ =version }" on an existing one,
   this will fail because opam will pin the new package as "dev" but the old one with
   the version of its last release. *)
let maybe_add_dev ~dir name =
  if Fpath.is_current_dir dir || not (String.contains (Fpath.basename dir) '.')
  then name ^ ".dev"
  else name

(* Group opam files by directory.
   e.g. ["a/a1.opam"; "a/a2.opam"; "b/b1.opam"] ->
        [("a", ["a/a1.opam"; "a/a2.opam"], ["a1.dev"; "a2.dev"]);
         ("b", ["b/b1.opam"], ["b1.dev"])
        ] *)
let group_opam_files =
  ListLabels.fold_left ~init:[] ~f:(fun acc x ->
      let item = Fpath.v x in
      let dir = Fpath.parent item in
      let pkg =
        Filename.basename x |> Filename.chop_extension |> maybe_add_dev ~dir
      in
      match acc with
      | (prev_dir, prev_items, pkgs) :: rest when Fpath.equal dir prev_dir ->
          (prev_dir, x :: prev_items, pkg :: pkgs) :: rest
      | _ -> (dir, [ x ], [ pkg ]) :: acc)

let mkdir dirs =
  let dirs =
    dirs
    |> List.filter_map (fun (dir, _, _) ->
           if Fpath.is_current_dir dir then None
           else Some (Filename.quote (Fpath.to_string dir)))
  in
  match dirs with
  | [] -> []
  | dirs -> [ Obuilder_spec.run "mkdir -p %s" (String.concat " " dirs) ]

(* Generate instructions to copy all the files in [items] into the
   image, creating the necessary directories first, and then pin them all. *)
let pin_opam_files ~network ?work_dir groups =
  if groups = [] then []
  else
    let open Obuilder_spec in
    mkdir groups
    @ (groups
      |> List.map (fun (dir, files, _) ->
             copy files
               ~dst:
                 (let dir =
                    Option.map
                      (fun work_dir -> Fpath.( // ) work_dir dir)
                      work_dir
                    |> Option.value ~default:dir
                  in
                  Fpath.to_string dir)))
    @ [
        groups
        |> List.concat_map (fun (dir, _, pkgs) ->
               pkgs
               |> List.map (fun pkg ->
                      let dir =
                        Option.map
                          (fun work_dir -> Fpath.( // ) work_dir dir)
                          work_dir
                        |> Option.value ~default:dir
                      in
                      Printf.sprintf "opam pin add -yn %s %s" pkg
                        (Filename.quote (Fpath.to_string dir))))
        |> String.concat " && \n"
        |> run ~network "%s";
      ]

(* Get the packages directly in "." *)
let rec get_root_opam_packages = function
  | [] -> []
  | (dir, _, pkgs) :: _ when Fpath.is_current_dir dir -> pkgs
  | _ :: rest -> get_root_opam_packages rest

let download_cache = "opam-archives"

(* -          Obuilder_spec.Cache.v download_cache ~target:"~/.opam/download-cache";
   +          Obuilder_spec.Cache.v download_cache
   +            ~target:"/Users/mac1000/.opam/download-cache";
   +          Obuilder_spec.Cache.v "homebrew"
   +            ~target:"/Users/mac1000/Library/Caches/Homebrew"; *)

let install_project_deps ~opam_version ~opam_files ~selection =
  let { Selection.packages; commit; variant; only_packages } = selection in
  match Variant.distro' variant with
  | None -> raise Exit
  | Some distro ->
      let groups = group_opam_files opam_files in
      let root_pkgs = get_root_opam_packages groups in
      let compatible_root_pkgs =
        if only_packages = [] then root_pkgs else only_packages
      in
      let non_root_pkgs =
        List.filter (fun pkg -> not (List.mem pkg root_pkgs)) packages
        |> String.concat " "
      in
      let network = [ "host" ] in
      let cache = Obuilder_spec_opam.cache distro in
      let distro_extras =
        match distro with
        | `Fedora _ ->
            [ Obuilder_spec.run ~network "sudo dnf install -y findutils" ]
            (* (we need xargs) *)
        | _ -> []
      in
      let home_dir =
        match Variant.os variant with
        | `Macos | `Windows | `Cygwin -> None
        | `Linux -> Some "/src"
      in
      let work_dir =
        match Variant.os variant with
        | `Macos | `Windows | `Cygwin -> Some (Fpath.v "./src/")
        | `Linux -> None
      in
      let open Obuilder_spec in
      Obuilder_spec_opam.set_personality (Variant.arch variant)
      @ [ env "CLICOLOR_FORCE" "1" ]
      @ [ env "OPAMCOLOR" "always" ]
      @ (match home_dir with
        | Some home_dir -> [ workdir home_dir ]
        | None -> [])
      @ distro_extras
      @ Obuilder_spec_opam.opam_init opam_version distro
      @ (match home_dir with
        | Some home_dir -> [ workdir home_dir; run "sudo chown opam /src" ]
        | None -> [])
      @ [
          run ~network ~cache
            "cd ~/opam-repository && (git cat-file -e %s || git fetch origin \
             master) && git reset -q --hard %s && git log --no-decorate -n1 \
             --oneline && opam update -u"
            commit commit;
        ]
      @ pin_opam_files ~network ?work_dir groups
      @ [ env "DEPS" non_root_pkgs; env "CI" "true"; env "OCAMLCI" "true" ]
      @ Obuilder_spec_opam.opam_depext ~network ~cache ~opam_version
          compatible_root_pkgs
      @ [ run ~network ~cache "opam install $DEPS" ]

let spec ~base ~opam_version ~opam_files ~selection =
  let open Obuilder_spec in
  let to_name x = OpamPackage.of_string x |> OpamPackage.name_to_string in
  let home_dir =
    match Variant.os selection.Selection.variant with
    | `Macos | `Windows | `Cygwin -> "./src"
    | `Linux -> "/src"
  in
  let only_packages =
    match selection.Selection.only_packages with
    | [] -> ""
    | pkgs -> " --only-packages=" ^ String.concat "," (List.map to_name pkgs)
  in
  let run_build =
    match Variant.os selection.Selection.variant with
    | `Linux | `Windows | `Cygwin ->
        run
          "opam exec -- dune build%s @install @check @runtest && rm -rf _build"
          only_packages
    | `Macos ->
        run
          "cd ./src && opam exec -- dune build%s @install @check @runtest && \
           rm -rf _build"
          only_packages
  in
  stage ~from:base
    (comment "%s" (Fmt.str "%a" Variant.pp selection.Selection.variant)
     :: user_unix ~uid:1000 ~gid:1000
     :: install_project_deps ~opam_version ~opam_files ~selection
    @ [ copy [ "." ] ~dst:home_dir; run_build ])
