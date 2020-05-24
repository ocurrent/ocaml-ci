module Store = Git_unix.Store
module Search = Git.Search.Make(Store)

open Lwt.Infix

type rejection = UserConstraint of OpamFormula.atom

type t = {
  env : string -> OpamVariable.variable_contents option;
  packages : OpamFile.OPAM.t OpamPackage.Version.Map.t OpamPackage.Name.Map.t;
  pins : (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t;
  constraints : OpamFormula.version_constraint OpamTypes.name_map;    (* User-provided constraints *)
  test : OpamPackage.Name.Set.t;
}

let load t pkg =
  let { OpamPackage.name; version } = pkg in
  match OpamPackage.Name.Map.find_opt name t.pins with
  | Some (_, opam) -> opam
  | None ->
    match OpamPackage.Name.Map.find_opt name t.packages with
    | None -> Fmt.failwith "Package %S not found" (OpamPackage.Name.to_string name)
    | Some versions ->
      match OpamPackage.Version.Map.find_opt version versions with
      | None -> Fmt.failwith "Package %S not found" (OpamPackage.to_string pkg)
      | Some opam -> opam

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

let candidates t name =
  match OpamPackage.Name.Map.find_opt name t.pins with
  | Some (version, _) -> [version, None]
  | None ->
    match OpamPackage.Name.Map.find_opt name t.packages with
    | None ->
      OpamConsole.log "opam-0install" "Package %S not found!" (OpamPackage.Name.to_string name);
      []
    | Some versions ->
      let user_constraints = user_restrictions t name in
      OpamPackage.Version.Map.bindings versions
      |> List.rev_map (fun (v, _) ->
          match user_constraints with
          | Some test when not (OpamFormula.check_version_formula (OpamFormula.Atom test) v) ->
            v, Some (UserConstraint (name, Some test))  (* Reject *)
          | _ -> v, None
        )

let pp_rejection f = function
  | UserConstraint x -> Fmt.pf f "Rejected by user-specified constraint %s" (OpamFormula.string_of_atom x)

let read_dir store hash =
  Store.read store hash >|= function
  | Error e -> Fmt.failwith "Failed to read tree: %a" Store.pp_error e
  | Ok (Store.Value.Tree tree) -> Some tree
  | Ok _ -> None

let read_package store pkg hash =
  Search.find store hash (`Path ["opam"]) >>= function
  | None -> Fmt.failwith "opam file not found for %s" (OpamPackage.to_string pkg)
  | Some hash ->
    Store.read store hash >|= function
    | Ok (Store.Value.Blob blob) -> OpamFile.OPAM.read_from_string (Store.Value.Blob.to_string blob)
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

let create ?(test=OpamPackage.Name.Set.empty) ?(pins=OpamPackage.Name.Map.empty) ~constraints ~env ~packages =
  { env; packages; pins; constraints; test }
