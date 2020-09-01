module Worker = Ocaml_ci_api.Worker
module Solver = Opam_0install.Solver.Make(Git_context)
module Store = Git_unix.Store

module Response = struct
  type ('a, 'b) result = ('a, 'b) Stdlib.result =
    | Ok of 'a
    | Error of 'b
  [@@deriving yojson]

  type selection = {
    packages : string list;
    post_packages : string list;
  }
  [@@deriving yojson]

  type t = (selection, [`Solve of string]) result [@@deriving yojson]
end

open Lwt.Infix

let env (vars : Worker.Vars.t) =
  Opam_0install.Dir_context.std_env
    ~arch:vars.arch
    ~os:vars.os
    ~os_distribution:vars.os_distribution
    ~os_version:vars.os_version
    ~os_family:vars.os_family
    ()

let parse_opam (name, contents) =
  let pkg = OpamPackage.of_string name in
  let opam = OpamFile.OPAM.read_from_string contents in
  OpamPackage.name pkg, (OpamPackage.version pkg, opam)

let solve ~packages ~pins ~root_pkgs (vars : Worker.Vars.t) =
  let ocaml_package = OpamPackage.Name.of_string vars.ocaml_package in
  let ocaml_version = OpamPackage.Version.of_string vars.ocaml_version in
  let context =
    Git_context.create
      ~packages
      ~pins
      ~env:(env vars)
      ~constraints:(OpamPackage.Name.Map.singleton ocaml_package (`Eq, ocaml_version))
      ~test:(OpamPackage.Name.Set.of_list root_pkgs)
  in
  let t0 = Unix.gettimeofday () in
  let r = Solver.solve context (ocaml_package :: root_pkgs) in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%.2f\n" (t1 -. t0);
  match r with
  | Ok sels -> Ok (Solver.packages_of_result sels)
  | Error diagnostics ->
    Error (Solver.diagnostics diagnostics)

(** Name of dependencies with the 'post' flag *)
let post_dependencies p : OpamPackage.Name.t list =
  let post_var = OpamVariable.Full.of_string "post" in
  let is_post f =
    OpamFormula.fold_left (fun acc -> function
        | OpamTypes.Filter f -> acc || List.mem post_var (OpamFilter.variables f)
        | _ -> acc) false f
  in
  OpamFormula.fold_left (fun acc (name, f) ->
      if is_post f then name :: acc else acc
    ) [] p.OpamFile.OPAM.depends

let process ~post_packages_names pkgs =
  let is_post pkg = List.mem pkg.OpamPackage.name post_packages_names in
  let serialize = List.map OpamPackage.to_string in
  let post_packages, packages = List.partition is_post pkgs in
  let packages, post_packages = serialize packages, serialize post_packages in
  { Response.packages; post_packages }

let main commit =
  let packages =
    Lwt_main.run begin
      Opam_repository.open_store () >>= fun store ->
      Git_context.read_packages store commit
    end
  in
  let rec aux () =
    match input_line stdin with
    | exception End_of_file -> ()
    | len ->
      let len = int_of_string len in
      let data = really_input_string stdin len in
      let request = Worker.Solve_request.of_yojson (Yojson.Safe.from_string data) |> Result.get_ok in
      let { Worker.Solve_request.opam_repository_commit; root_pkgs; pinned_pkgs; platforms } = request in
      let opam_repository_commit = Store.Hash.of_hex opam_repository_commit in
      assert (Store.Hash.equal opam_repository_commit commit);
      let root_pkgs = List.map parse_opam root_pkgs in
      let pinned_pkgs = List.map parse_opam pinned_pkgs in
      let pins =
        root_pkgs @ pinned_pkgs
        |> OpamPackage.Name.Map.of_list
      in
      let post_packages_names =
        List.fold_left (fun acc (_, (_, p)) -> post_dependencies p @ acc) [] root_pkgs
      in
      let root_pkgs = List.map fst root_pkgs in
      platforms |> List.iter (fun (_id, platform) ->
          let resp =
            match solve ~packages ~pins ~root_pkgs platform with
            | Ok pkgs -> Ok (process ~post_packages_names pkgs)
            | Error msg -> Error (`Solve msg)
          in
          let msg =
            Response.to_yojson resp |> Yojson.Safe.to_string
          in
          Printf.printf "%d\n%s%!" (String.length msg) msg;
        );
      aux ()
  in
  aux ()

let main commit =
  try main commit
  with ex ->
    Fmt.epr "solver bug: %a@." Fmt.exn ex;
    let msg =
      match ex with
      | Failure msg -> msg
      | ex -> Printexc.to_string ex
    in
    let msg = "!" ^ msg in
    Printf.printf "0.0\n%d\n%s%!" (String.length msg) msg;
    raise ex
