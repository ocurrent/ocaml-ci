open Lwt.Infix

module Worker = Ocaml_ci_api.Worker
module Selection = Worker.Selection

(* Send [request] to [worker] and read the reply. *)
let process ~id request worker =
  let request_str = Worker.Solve_request.to_yojson request |> Yojson.Safe.to_string in
  let request_str = Printf.sprintf "%d\n%s" (String.length request_str) request_str in
  Lwt_io.write worker#stdin request_str >>= fun () ->
  Lwt_io.read_line worker#stdout >>= fun time ->
  Lwt_io.read_line worker#stdout >>= fun len ->
  let len = int_of_string len in
  Lwt_io.read ~count:len worker#stdout >|= fun results ->
  if String.length results = 0 then Fmt.failwith "No output from solve worker!";
  match results.[0] with
  | '+' ->
    Log.info (fun f -> f "%s: found solution in %s s" id time);
    let packages =
      Astring.String.with_range ~first:1 results
      |> Astring.String.cuts ~sep:" "
    in
    Ok packages
  | '-' ->
    Log.info (fun f -> f "%s: eliminated all possibilities in %s s" id time);
    let msg = results |> Astring.String.with_range ~first:1 in
    Error msg
  | _ ->
    Fmt.failwith "BUG: bad output: %s" results

(* Handle a request by distributing it among the worker processes and then aggregating their responses. *)
let handle ~pool request =
  let { Worker.Solve_request.opam_repository; platforms; root_pkgs; pinned_pkgs } = request in
  let opam_repository = Opam_repository.of_dir opam_repository in
  let root_pkgs = List.map fst root_pkgs in
  let pinned_pkgs = List.map fst pinned_pkgs in
  let pins =
    root_pkgs @ pinned_pkgs
    |> List.map OpamPackage.of_string
    |> List.map OpamPackage.name
    |> OpamPackage.Name.Set.of_list
  in
  Log.info (fun f -> f "Solving for %a" Fmt.(list ~sep:comma string) root_pkgs);
  platforms |> Lwt_list.map_p (fun p ->
      let id = fst p in
      let slice = { request with platforms = [p] } in
      Lwt_pool.use pool (process ~id slice) >>= function
      | Error _ as e -> Lwt.return (id, e)
      | Ok packages ->
        let repo_packages =
          packages |> List.filter_map (fun pkg ->
              let pkg = OpamPackage.of_string pkg in
              if OpamPackage.Name.Set.mem pkg.name pins then None
              else Some pkg
            )
        in
        Opam_repository.oldest_commit_with opam_repository repo_packages >|= fun commit ->
        id, Ok { Worker.Selection.id; packages; commit }
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

(* Read a request from stdin, process it, and write the response on stdout. *)
let run_toplevel pool =
  match Worker.Solve_request.of_yojson (Yojson.Safe.from_channel stdin) with
  | Error msg -> Fmt.failwith "Bad request: %s" msg
  | Ok request ->
    Lwt.catch
      (fun () -> handle ~pool request >|= Result.ok)
      (function
        | Failure msg -> Lwt_result.fail (`Msg msg)
        | ex -> Lwt.return (Fmt.error_msg "%a" Fmt.exn ex)
      )
    >|= fun response ->
    Yojson.Safe.to_channel stdout (Worker.Solve_response.to_yojson response)

let main ~self args =
  Logs.(set_level (Some Info));
  Logs.set_reporter Log.reporter;
  match args with
  | [] ->
    let n_workers = 20 in
    Lwt_main.run begin
      let pool = Lwt_pool.create n_workers (fun () ->
          let cmd = ("", Array.of_list (self @ ["--worker"])) in
          Lwt.return (Lwt_process.open_process cmd)
        ) in
      run_toplevel pool
    end
  | ["--worker"] -> Solver.main ()
  | args -> Fmt.failwith "Usage: %a (got %a)" Fmt.(list string) self Fmt.(list (quote string)) args
