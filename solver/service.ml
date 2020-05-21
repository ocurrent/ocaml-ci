open Lwt.Infix
open Capnp_rpc_lwt

module Worker = Ocaml_ci_api.Worker
module Log = Ocaml_ci_api.Solver.Log
module Selection = Worker.Selection

(* Send [request] to [worker] and read the reply. *)
let process ~log ~id request worker =
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
    Log.info log "%s: found solution in %s s" id time;
    let packages =
      Astring.String.with_range ~first:1 results
      |> Astring.String.cuts ~sep:" "
    in
    Ok packages
  | '-' ->
    Log.info log "%s: eliminated all possibilities in %s s" id time;
    let msg = results |> Astring.String.with_range ~first:1 in
    Error msg
  | _ ->
    Fmt.failwith "BUG: bad output: %s" results

(* Handle a request by distributing it among the worker processes and then aggregating their responses. *)
let handle ~pool ~log request =
  let { Worker.Solve_request.opam_repository; platforms; root_pkgs; pinned_pkgs } = request in
  let opam_repository = Opam_repository.of_dir opam_repository in
  let root_pkgs = List.map fst root_pkgs in
  let pinned_pkgs = List.map fst pinned_pkgs in
  let pins =
    root_pkgs @ pinned_pkgs
    |> List.map (fun pkg -> OpamPackage.name (OpamPackage.of_string pkg))
    |> OpamPackage.Name.Set.of_list
  in
  Log.info log "Solving for %a" Fmt.(list ~sep:comma string) root_pkgs;
  platforms |> Lwt_list.map_p (fun p ->
      let id = fst p in
      let slice = { request with platforms = [p] } in
      Lwt_pool.use pool (process ~log ~id slice) >>= function
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
      Log.info log "= %s =" id;
      match result with
      | Ok result ->
        Log.info log "-> @[<hov>%a@]" Fmt.(list ~sep:sp string) result.Selection.packages;
        Log.info log "(valid since opam-repository commit %s)" result.Selection.commit;
        Some result
      | Error msg ->
        Log.info log "%s" msg;
        None
    )

let v ~pool =
  let module X = Ocaml_ci_api.Raw.Service.Solver in
  X.local @@ object
    inherit X.service

    method solve_impl params release_param_caps =
      let open X.Solve in
      let request = Params.request_get params in
      let log = Params.log_get params in
      release_param_caps ();
      match log with
      | None -> Service.fail "Missing log argument!"
      | Some log ->
        match Worker.Solve_request.of_yojson (Yojson.Safe.from_string request) with
        | Error msg -> Service.fail "Bad JSON in request: %s" msg
        | Ok request ->
          Capnp_rpc_lwt.Service.return_lwt @@ fun () ->
          Lwt.catch
            (fun () -> handle ~log ~pool request >|= Result.ok)
            (function
              | Failure msg -> Lwt_result.fail (`Msg msg)
              | ex -> Lwt.return (Fmt.error_msg "%a" Fmt.exn ex)
            )
          >|= fun selections ->
          let json = Yojson.Safe.to_string (Worker.Solve_response.to_yojson selections) in
          let response, results = Capnp_rpc_lwt.Service.Response.create Results.init_pointer in
          Results.response_set results json;
          Ok response
  end
