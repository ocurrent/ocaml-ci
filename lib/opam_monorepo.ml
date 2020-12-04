type info = string * OpamFile.OPAM.t

type config = {
  package : string;
  dune_version : string;
  monorepo_version : string;
}
[@@deriving yojson, ord]

type selection = Variant.t * config [@@deriving yojson]

let only_lockfile_in ~dir =
  let opam_locked = ".opam.locked" in
  let lock_files =
    Sys.readdir (Fpath.to_string dir)
    |> Array.to_list
    |> List.filter (Astring.String.is_suffix ~affix:opam_locked)
  in
  match lock_files with
  | [ lock_file ] ->
      let last = String.length lock_file - String.length opam_locked - 1 in
      let base = Astring.String.with_index_range ~last lock_file in
      Some (base, lock_file)
  | _ -> None

let guard = function true -> Some () | false -> None

let file_exists_in ~dir ~name =
  let path = Filename.concat (Fpath.to_string dir) name in
  Sys.file_exists path

let x_opam_monorepo_version opam =
  try
    OpamFile.OPAM.extended opam "x-opam-monorepo-version" (function
      | String (_, s) -> s
      | _ -> assert false)
  with Invalid_argument _ -> None

let detect ~dir =
  let ( let* ) = Option.bind in
  let* package, lock_file_path = only_lockfile_in ~dir in
  let opam_file_path = package ^ ".opam" in
  let* () = guard (file_exists_in ~dir ~name:opam_file_path) in
  let* () = guard (file_exists_in ~dir ~name:"dune-project") in
  let full_path = Filename.concat (Fpath.to_string dir) lock_file_path in
  let lock_file_contents =
    OpamFile.OPAM.read (OpamFile.make (OpamFilename.of_string full_path))
  in
  let* _ = x_opam_monorepo_version lock_file_contents in
  Some (package, lock_file_contents)

let packages_in_depends f =
  let get_atom = function OpamFormula.Atom a -> a | _ -> assert false in
  OpamFormula.ands_to_list f |> List.map get_atom

let version_of_equal_constraint f =
  match f with
  | OpamFormula.Atom (OpamTypes.Constraint (`Eq, OpamTypes.FString s)) -> s
  | _ -> assert false

let opam_monorepo_dep_version ~lock_file ~package =
  OpamFile.OPAM.depends lock_file
  |> packages_in_depends
  |> List.assoc (OpamPackage.Name.of_string package)
  |> version_of_equal_constraint

let ocaml_major_version vars =
  Ocaml_version.with_just_major_and_minor
    (Ocaml_version.of_string_exn vars.Ocaml_ci_api.Worker.Vars.ocaml_version)

let find_compiler ~platforms ~version =
  let v =
    Ocaml_version.with_just_major_and_minor
      (Ocaml_version.of_string_exn version)
  in
  let matches_compiler (variant, vars) =
    if Ocaml_version.equal v (ocaml_major_version vars) then Some variant
    else None
  in
  List.find_map matches_compiler platforms

let selections ~platforms ~info:(package, lock_file) =
  let ocaml_version = opam_monorepo_dep_version ~lock_file ~package:"ocaml" in
  let dune_version = opam_monorepo_dep_version ~lock_file ~package:"dune" in
  let monorepo_version = x_opam_monorepo_version lock_file |> Option.get in
  let spec = { package; dune_version; monorepo_version } in
  Option.map
    (fun variant -> (variant, spec))
    (find_compiler ~platforms ~version:ocaml_version)

let install_opam_monorepo ~network ~cache ~monorepo_version =
  match monorepo_version with
  | "0.1" ->
      let monorepo_commit = "6b6c1b8173afab88933762f6b5ee82a070b2d715" in
      let open Obuilder_spec in
      [
        run ~network ~cache
          "opam pin add -n https://github.com/ocamllabs/duniverse.git#%s"
          monorepo_commit;
        run ~network ~cache "opam depext --update -y opam-monorepo";
        run ~network ~cache "opam install opam-monorepo";
      ]
  | v -> Printf.ksprintf failwith "unknown opam-monorepo version %S" v

let install_dune ~network ~cache ~dune_version =
  let open Obuilder_spec in
  [ run ~network ~cache "opam install dune.%s" dune_version ]

let install_tools ~network ~cache ~dune_version ~monorepo_version =
  install_dune ~network ~cache ~dune_version
  @ install_opam_monorepo ~network ~cache ~monorepo_version

let install_depexts ~network ~cache ~package =
  let open Obuilder_spec in
  [
    run ~network ~cache "opam pin -n add %s . --locked" package;
    run ~network ~cache "opam depext --update -y %s" package;
    run ~network ~cache "opam pin -n remove %s" package;
  ]

let spec ~base ~repo ~spec ~variant =
  let { package; dune_version; monorepo_version } = spec in
  let opam_file = package ^ ".opam" in
  let lock_file = package ^ ".opam.locked" in
  let download_cache =
    Obuilder_spec.Cache.v Opam_build.download_cache
      ~target:"/home/opam/.opam/download-cache"
  in
  let dune_cache = Duniverse_build.build_cache repo in
  let network = [ "host" ] in
  let dune_project = "dune-project" in
  let open Obuilder_spec in
  stage ~from:base
  @@ [ comment "%s" (Variant.to_string variant); user ~uid:1000 ~gid:1000 ]
  @ install_tools ~network ~cache:[ download_cache ] ~dune_version
      ~monorepo_version
  @ [
      workdir "/src";
      run "sudo chown opam /src";
      copy [ dune_project; opam_file; lock_file ] ~dst:"/src/";
    ]
  @ install_depexts ~network ~cache:[ download_cache ] ~package
  @ [
      run ~network ~cache:[ download_cache ] "opam exec -- opam monorepo pull";
      copy [ "." ] ~dst:"/src/";
      run ~cache:[ dune_cache ] "opam exec -- dune build @install";
      run ~cache:[ dune_cache ] "opam exec -- dune runtest";
    ]
