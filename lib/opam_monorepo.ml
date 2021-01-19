type info = string * OpamFile.OPAM.t

type lock_file_version = V0_1 | V0_2 [@@deriving yojson, ord]

type config = {
  package : string;
  selection : Selection.t;
  lock_file_version : lock_file_version;
}
[@@deriving yojson, ord]

let variant_of_config c = c.selection.variant

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
    OpamFile.OPAM.extended opam "x-opam-monorepo-version"
      (OpamPp.parse OpamFormat.V.string ~pos:OpamTypesBase.pos_null)
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

let lock_file_version_of_string = function
  | "0.1" -> V0_1
  | "0.2" -> V0_2
  | v -> Printf.ksprintf failwith "unknown x-opam-monorepo-version %S" v

let exactly v = Printf.sprintf {|{ = "%s" }|} v

let between a b = Printf.sprintf {|{ >= "%s" & < "%s" }|} a b

let plugin_version = function
  | V0_1 -> between "0.1.0" "0.3.0"
  | V0_2 -> between "0.2.0" "0.3.0"

let opam_dep_file packages =
  let lines =
    [ {|opam-version: "2.0"|}; {|depends: [|} ]
    @ List.map (fun (pkg, ver) -> Printf.sprintf {|  "%s" %s|} pkg ver) packages
    @ [ {|]|} ]
  in
  lines |> List.map (fun s -> s ^ "\n") |> String.concat ""

let selection ~info:(package, lock_file) ~platforms ~solve =
  let open Lwt_result.Infix in
  let ocaml_version = opam_monorepo_dep_version ~lock_file ~package:"ocaml" in
  let dune_version = opam_monorepo_dep_version ~lock_file ~package:"dune" in
  let lock_file_version =
    x_opam_monorepo_version lock_file
    |> Option.get |> lock_file_version_of_string
  in
  let monorepo_version = plugin_version lock_file_version in
  let deps_package = "opam-monorepo-deps.dev" in
  let root_pkgs =
    [
      ( deps_package,
        opam_dep_file
          [
            ("ocaml", exactly ocaml_version);
            ("dune", exactly dune_version);
            ("opam-monorepo", monorepo_version);
          ] );
    ]
  in
  let platforms =
    let version = Ocaml_version.of_string_exn ocaml_version in
    platforms
    |> List.filter (fun (_, vars) ->
           Platform.compiler_matches_major_and_minor ~version vars)
  in
  solve ~root_pkgs ~pinned_pkgs:[] ~platforms >|= fun workers ->
  let selection =
    List.hd workers |> Selection.remove_package ~package:deps_package
  in
  `Opam_monorepo { package; selection; lock_file_version }

let install_depexts ~network ~cache ~package ~lock_file_version =
  let open Obuilder_spec in
  match lock_file_version with
  | V0_1 ->
      [
        run ~network ~cache "opam pin -n add %s . --locked" package;
        run ~network ~cache "opam depext --update -y %s" package;
        run ~network ~cache "opam pin -n remove %s" package;
      ]
  | V0_2 ->
      [
        run ~network ~cache
          "sudo apt-get -y update && sudo apt-get -y install $(opam show -f \
           depexts ./%s.opam.locked)"
          package;
      ]

let spec ~base ~repo ~config ~variant =
  let { package; selection; lock_file_version } = config in
  let opam_file = package ^ ".opam" in
  let lock_file = package ^ ".opam.locked" in
  let download_cache =
    Obuilder_spec.Cache.v Opam_build.download_cache
      ~target:"/home/opam/.opam/download-cache"
  in
  let dune_cache = Dune_build_cache.for_repo repo in
  let network = [ "host" ] in
  let dune_project = "dune-project" in
  let open Obuilder_spec in
  stage ~from:base
  @@ [ comment "%s" (Variant.to_string variant); user ~uid:1000 ~gid:1000 ]
  @ Opam_build.install_project_deps ~opam_files:[] ~selection
  @ [
      workdir "/src";
      run "sudo chown opam /src";
      copy [ dune_project; opam_file; lock_file ] ~dst:"/src/";
    ]
  @ install_depexts ~network ~cache:[ download_cache ] ~package
      ~lock_file_version
  @ [
      run ~network ~cache:[ download_cache ] "opam exec -- opam monorepo pull";
      copy [ "." ] ~dst:"/src/";
      run ~cache:[ dune_cache ] "opam exec -- dune build @install";
      run ~cache:[ dune_cache ] "opam exec -- dune runtest";
    ]
