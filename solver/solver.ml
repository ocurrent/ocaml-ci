open Lwt.Infix

let src = Logs.Src.create "ocaml_ci_solver" ~doc:"ocaml-ci dependency solver"
module Log = (val Logs.src_log src : Logs.LOG)

module Worker = Ocaml_ci_api.Worker
module Solver = Opam_0install.Solver.Make(Opam_0install.Dir_context)
module Selection = Worker.Selection

let ( / ) = Filename.concat

let env (vars : Worker.Vars.t) =
  Opam_0install.Dir_context.std_env
    ~arch:vars.arch
    ~os:vars.os
    ~os_distribution:vars.os_distribution
    ~os_version:vars.os_version
    ~os_family:vars.os_family

let ocaml_name = OpamPackage.Name.of_string "ocaml"

(* Use "git-log" to find the oldest commit with these package versions.
   This avoids invalidating the Docker build cache on every update to opam-repository. *)
let oldest_commit_with ~opam_repository ~pins pkgs =
  let paths =
    pkgs |> List.filter_map (fun pkg ->
        let { OpamPackage.name; version } = OpamPackage.of_string pkg in
        if OpamPackage.Name.Map.mem name pins then None
        else (
          let name = OpamPackage.Name.to_string name in
          let version = OpamPackage.Version.to_string version in
          Some (Printf.sprintf "%s/%s.%s" name name version)
        )
      )
  in
  let cmd = "git" :: "-C" :: opam_repository / "packages" :: "log" :: "-n" :: "1" :: "--format=format:%H" :: paths in
  let cmd = ("", Array.of_list cmd) in
  Process.pread cmd >|= String.trim

let version_2 = OpamVersion.of_string "2"

let parse_opam (name, contents) =
  let pkg = OpamPackage.of_string name in
  let opam = OpamFile.OPAM.read_from_string contents in
  let opam_version = OpamFile.OPAM.opam_version opam in
  if OpamVersion.compare opam_version version_2 < 0 then
    Fmt.failwith "Package %S uses unsupported opam version %s (need >= 2)" name (OpamVersion.to_string opam_version);
  OpamPackage.name pkg, (OpamPackage.version pkg, opam)

let run_child ~opam_repository ~pins ~root_pkgs (vars : Worker.Vars.t) =
  let ocaml_version = OpamPackage.Version.of_string vars.ocaml_version in
  let context =
    Opam_0install.Dir_context.create (opam_repository / "packages")
      ~pins
      ~env:(env vars)
      ~constraints:(OpamPackage.Name.Map.singleton ocaml_name (`Eq, ocaml_version))
      ~test:(OpamPackage.Name.Set.of_list root_pkgs)
  in
  let t0 = Unix.gettimeofday () in
  let r = Solver.solve context root_pkgs in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%.2f\n" (t1 -. t0);
  match r with
  | Ok sels ->
    let pkgs = Solver.packages_of_result sels in
    let packages = List.map OpamPackage.to_string pkgs in
    Printf.printf "+%s%!" @@ String.concat " " packages
  | Error diagnostics ->
    let msg = Solver.diagnostics diagnostics in
    Printf.printf "-%s%!" msg

let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

let check_exit_status = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code -> Fmt.failwith "Child returned error exit status %d" code
  | Unix.WSIGNALED signal -> Fmt.failwith "Child aborted (signal %d)" signal
  | Unix.WSTOPPED signal -> Fmt.failwith "Child is currently stopped (signal %d)" signal

let spawn_child run (id, vars) =
  let r, w = Unix.pipe ~cloexec:true () in
  match Unix.fork () with
  | 0 -> (* We are the child *)
    begin
      try
        Unix.close r;
        Unix.dup2 w Unix.stdout;
        run vars;
        exit 0
      with ex ->
        Printf.printf "-%s\n%!" (Printexc.to_string ex);
        exit 1
    end
  | child ->
    Unix.close w;
    id, child, r

let pp_name = Fmt.of_to_string OpamPackage.Name.to_string

let solve { Worker.Solve_request.opam_repository; root_pkgs; pinned_pkgs; platforms } =
  let root_pkgs = List.map parse_opam root_pkgs in
  let pinned_pkgs = List.map parse_opam pinned_pkgs in
  let pins =
    root_pkgs @ pinned_pkgs
    |> OpamPackage.Name.Map.of_list
  in
  let root_pkgs = List.map fst root_pkgs in
  Log.info (fun f -> f "Solving for %a" (Fmt.(list ~sep:comma) pp_name) root_pkgs);
  let jobs = List.map (spawn_child (run_child ~opam_repository ~pins ~root_pkgs)) platforms in
  let results =
    jobs |> List.map (fun (id, pid, from_child) ->
        let buf = Bytes.create 4096 in
        let results = Buffer.create 4096 in
        let rec read () =
          let got = Unix.read from_child buf 0 (Bytes.length buf) in
          if got > 0 then (
            Buffer.add_subbytes results buf 0 got;
            read ()
          ) else (
            Unix.close from_child;
            Buffer.contents results
          )
        in
        let results = read () in
        waitpid_non_intr pid |> snd |> check_exit_status;
        match Astring.String.cut ~sep:"\n" results with
        | None -> Fmt.failwith "Missing newline in solver results: %S" results
        | Some (time, results) ->
          if String.length results = 0 then Fmt.failwith "No output from solve worker!";
          match results.[0] with
          | '+' ->
            Log.info (fun f -> f "%s: found solution in %s s" id time);
            let packages =
              Astring.String.with_range ~first:1 results
              |> Astring.String.cuts ~sep:" "
            in
            (id, Ok packages)
          | '-' ->
            Log.info (fun f -> f "%s: eliminated all possibilities in %s s" id time);
            let msg = results |> Astring.String.with_range ~first:1 in
            (id, Error msg)
          | _ ->
            Fmt.failwith "BUG: bad output: %s" results
      ) in
  (* Now all the sub-processes are gone, we can safely start Lwt. *)
  Lwt_main.run begin
    results
    |> Lwt_list.map_p (fun (id, output) ->
        match output with
        | Ok packages ->
          oldest_commit_with packages ~opam_repository ~pins >|= fun commit ->
          id, Ok { Selection.id; packages; commit }
        | Error _ as e -> Lwt.return (id, e)
      )
    >|= List.filter_map (fun (id, result) ->
        Log.info (fun f -> f "= %s =" id);
        match result with
        | Ok result ->
          Log.info (fun f -> f "-> @[<hov>%a@]" Fmt.(list ~sep:sp string) result.Selection.packages);
          Log.info (fun f -> f "(valid since opam-repository commit %s)" result.Selection.commit);
          Some result
        | Error msg ->
          Log.info (fun f -> f "%s" msg);
          None
      )
  end
