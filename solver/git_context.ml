module Store = Git_unix.Store
module Search = Git.Search.Make (Digestif.SHA1) (Store)

open Lwt.Infix

type rejection =
  | UserConstraint of OpamFormula.atom
  | Unavailable

type t = {
  env : string -> OpamVariable.variable_contents option;
  packages : OpamFile.OPAM.t OpamPackage.Version.Map.t OpamPackage.Name.Map.t;
  pins : (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t;
  constraints : OpamFormula.version_constraint OpamTypes.name_map;    (* User-provided constraints *)
  test : OpamPackage.Name.Set.t;
}

let ocaml_beta_pkg = OpamPackage.of_string "ocaml-beta.enabled"

(* From https://github.com/ocaml/ocaml-beta-repository/blob/master/packages/ocaml-beta/ocaml-beta.enabled/opam *)
let ocaml_beta_opam = OpamFile.OPAM.read_from_string {|
opam-version: "2.0"
maintainer: "platform@lists.ocaml.org"
bug-reports: "https://github.com/ocaml/ocaml/issues"
authors: [
  "Xavier Leroy"
  "Damien Doligez"
  "Alain Frisch"
  "Jacques Garrigue"
  "Didier Rémy"
  "Jérôme Vouillon"
]
homepage: "https://ocaml.org"
synopsis: "OCaml beta releases enabled"
description: "Virtual package enabling the installation of OCaml beta releases."
flags: avoid-version
|}

let user_restrictions t name =
  OpamPackage.Name.Map.find_opt name t.constraints

let dev = OpamPackage.Version.of_string "dev"

let env t pkg v =
  if List.mem v OpamPackageVar.predefined_depends_variables then None
  else match OpamVariable.Full.to_string v with
    | "version" -> Some (OpamTypes.S (OpamPackage.version_to_string pkg))
    | x -> t.env x

let filter_deps t pkg f =
  let dev = OpamPackage.Version.compare (OpamPackage.version pkg) dev = 0 in
  let test = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.test in
  f
  |> OpamFilter.partial_filter_formula (env t pkg)
  |> OpamFilter.filter_deps ~build:true ~post:true ~test ~doc:false ~dev ~default:false

let filter_available t pkg opam =
  let available = OpamFile.OPAM.available opam in
  match OpamFilter.eval ~default:(B false) (env t pkg) available with
  | B true -> Ok opam
  | B false -> Error Unavailable
  | _ ->
    OpamConsole.error "Available expression not a boolean: %s" (OpamFilter.to_string available);
    Error Unavailable

let version_compare (v1, opam1) (v2, opam2) =
  let avoid1 = List.mem OpamTypes.Pkgflag_AvoidVersion (OpamFile.OPAM.flags opam1) in
  let avoid2 = List.mem OpamTypes.Pkgflag_AvoidVersion (OpamFile.OPAM.flags opam2) in
  if avoid1 = avoid2 then
    OpamPackage.Version.compare v1 v2
  else if avoid1 then -1 else 1

let candidates t name =
  match OpamPackage.Name.Map.find_opt name t.pins with
  | Some (version, opam) ->
    let pkg = OpamPackage.create name version in
    [version, filter_available t pkg opam]
  | None ->
    match OpamPackage.Name.Map.find_opt name t.packages with
    | None ->
      OpamConsole.log "opam-0install" "Package %S not found!" (OpamPackage.Name.to_string name);
      []
    | Some versions ->
      let versions =
        if OpamPackage.Name.compare name (OpamPackage.name ocaml_beta_pkg) = 0 then 
          OpamPackage.Version.Map.add (OpamPackage.version ocaml_beta_pkg) ocaml_beta_opam versions
        else versions
      in
      let user_constraints = user_restrictions t name in
      OpamPackage.Version.Map.bindings versions
      |> List.fast_sort version_compare
      |> List.rev_map (fun (v, opam) ->
          match user_constraints with
          | Some test when not (OpamFormula.check_version_formula (OpamFormula.Atom test) v) ->
            v, Error (UserConstraint (name, Some test))
          | _ ->
            let pkg = OpamPackage.create name v in
            v, filter_available t pkg opam
        )

let pp_rejection f = function
  | UserConstraint x -> Fmt.pf f "Rejected by user-specified constraint %s" (OpamFormula.string_of_atom x)
  | Unavailable -> Fmt.string f "Availability condition not satisfied"

let read_dir store hash =
  Store.read store hash >|= function
  | Error e -> Fmt.failwith "Failed to read tree: %a" Store.pp_error e
  | Ok (Git.Value.Tree tree) -> Some tree
  | Ok _ -> None

let read_package store pkg hash =
  Search.find store hash (`Path ["opam"]) >>= function
  | None -> Fmt.failwith "opam file not found for %s" (OpamPackage.to_string pkg)
  | Some hash ->
    Store.read store hash >|= function
    | Ok (Git.Value.Blob blob) -> OpamFile.OPAM.read_from_string (Store.Value.Blob.to_string blob)
    | _ -> Fmt.failwith "Bad Git object type for %s!" (OpamPackage.to_string pkg)

(* Get a map of the versions inside [entry] (an entry under "packages") *)
let read_versions store (entry : Store.Value.Tree.entry) =
  read_dir store entry.node >>= function
  | None -> Lwt.return_none
  | Some tree ->
    Store.Value.Tree.to_list tree |> Lwt_list.fold_left_s (fun acc (entry : Store.Value.Tree.entry) ->
        match OpamPackage.of_string_opt entry.name with
        | Some pkg -> read_package store pkg entry.node >|= fun opam -> OpamPackage.Version.Map.add pkg.version opam acc
        | None ->
          OpamConsole.log "opam-0install" "Invalid package name %S" entry.name;
          Lwt.return acc
      ) OpamPackage.Version.Map.empty
    >|= fun versions -> Some versions

let read_packages store commit =
  Search.find store commit (`Commit (`Path ["packages"])) >>= function
  | None -> Fmt.failwith "Failed to find packages directory!"
  | Some tree_hash ->
    read_dir store tree_hash >>= function
    | None -> Fmt.failwith "'packages' is not a directory!"
    | Some tree ->
      Store.Value.Tree.to_list tree |> Lwt_list.fold_left_s (fun acc (entry : Store.Value.Tree.entry) ->
          match OpamPackage.Name.of_string entry.name with
          | exception ex ->
            OpamConsole.log "opam-0install" "Invalid package name %S: %s" entry.name (Printexc.to_string ex);
            Lwt.return acc
          | name ->
            read_versions store entry >|= function
            | None -> acc
            | Some versions -> OpamPackage.Name.Map.add name versions acc
        ) OpamPackage.Name.Map.empty

let create ?(test=OpamPackage.Name.Set.empty) ?(pins=OpamPackage.Name.Map.empty) ~constraints ~env ~packages () =
  { env; packages; pins; constraints; test }
