open Lwt.Infix
open Capnp_rpc_lwt

module Worker = Ocaml_ci_api.Worker
module Log = Ocaml_ci_api.Solver.Log
module Selection = Worker.Selection
module Store = Git_unix.Store

module Epoch : sig
  type t
  (* An Epoch handles all requests for a single opam-repository HEAD commit. *)

  val create : n_workers:int -> create_worker:(Git_unix.Store.Hash.t -> Lwt_process.process) -> Store.Hash.t -> t Lwt.t
  val handle : log:Ocaml_ci_api.Solver.Log.t -> Worker.Solve_request.t -> t -> Selection.t list Lwt.t
  val dispose : t -> unit Lwt.t
end = struct
  type t = Lwt_process.process Lwt_pool.t

  let validate (worker : Lwt_process.process) =
    match Lwt.state worker#status with
    | Lwt.Sleep -> Lwt.return true
    | Lwt.Fail ex -> Lwt.fail ex
    | Lwt.Return status ->
      Format.eprintf "Worker %d is dead (%a) - removing from pool@." worker#pid Process.pp_status status;
      Lwt.return false

  let dispose (worker : Lwt_process.process) =
    let pid = worker#pid in
    Fmt.epr "Terminating worker %d@." pid;
    worker#terminate;
    worker#status >|= fun _ ->
    Fmt.epr "Worker %d finished@." pid

  let create ~n_workers ~create_worker hash =
    begin
      Opam_repository.open_store () >>= fun store ->
      Store.mem store hash >>= function
      | true -> Lwt.return_unit
      | false ->
        Fmt.pr "Need to update opam-repository to get new commit %a@." Store.Hash.pp hash;
        Opam_repository.fetch () >>= fun () ->
        Opam_repository.open_store () >>= fun new_store ->
        Store.mem new_store hash >>= function
        | false -> Fmt.failwith "Still missing commit after update!"
        | true -> Lwt.return_unit
    end >|= fun () ->
    Lwt_pool.create n_workers ~validate ~dispose (fun () -> Lwt.return (create_worker hash))

  let dispose = Lwt_pool.clear

  (* Send [request] to [worker] and read the reply. *)
  let process ~log ~id request worker =
    let request_str = Worker.Solve_request.to_yojson request |> Yojson.Safe.to_string in
    let request_str = Printf.sprintf "%d\n%s" (String.length request_str) request_str in
    Lwt_io.write worker#stdin request_str >>= fun () ->
    Lwt_io.read_line worker#stdout >>= fun time ->
    Lwt_io.read_line worker#stdout >>= fun len ->
    match Astring.String.to_int len with
    | None ->
      Fmt.failwith "Bad frame from worker: time=%S len=%S" time len
    | Some len ->
      let buf = Bytes.create len in
      Lwt_io.read_into_exactly worker#stdout buf 0 len >|= fun () ->
      let results = Bytes.unsafe_to_string buf in
      match Solver.Response.of_yojson (Yojson.Safe.from_string results) with
      | Ok response ->
        Log.info log "%s: found solution in %s s" id time;
        response
      | Error msg ->
        Fmt.failwith "BUG: bad response from solver: %s\n%s" msg results

  let handle ~log request t =
    let { Worker.Solve_request.opam_repository_commit; platforms; root_pkgs; pinned_pkgs } = request in
    let opam_repository_commit = Store.Hash.of_hex opam_repository_commit in
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
        Lwt_pool.use t (process ~log ~id slice) >>= function
        | Error _ as e -> Lwt.return (id, e)
        | Ok { Solver.Response.packages; post_packages } ->
          let repo_packages =
            packages |> List.filter_map (fun pkg ->
                let pkg = OpamPackage.of_string pkg in
                if OpamPackage.Name.Set.mem pkg.name pins then None
                else Some pkg
              )
          in
          Opam_repository.oldest_commit_with repo_packages ~from:opam_repository_commit >|= fun commit ->
          id, Ok { Worker.Selection.id; packages; post_packages; commit }
      )
    >|= List.filter_map (fun (id, result) ->
        Log.info log "= %s =" id;
        match result with
        | Ok result ->
          Log.info log "-> @[<hov>%a@]" Fmt.(list ~sep:sp string) result.Selection.packages;
          Log.info log "(valid since opam-repository commit %s)" result.Selection.commit;
          Some result
        | Error (`Solve msg) ->
          Log.info log "%s" msg;
          None
      )
end

(* Handle a request by distributing it among the worker processes and then aggregating their responses. *)
let handle t ~log (request : Worker.Solve_request.t) =
  Epoch_lock.with_epoch t request.opam_repository_commit (Epoch.handle ~log request)

let v ~n_workers ~create_worker =
  Opam_repository.clone () >|= fun () ->
  let create hash = Epoch.create ~n_workers ~create_worker (Store.Hash.of_hex hash) in
  let t = Epoch_lock.v ~create ~dispose:Epoch.dispose () in
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
        Capnp_rpc_lwt.Service.return_lwt @@ fun () ->
        Capability.with_ref log @@ fun log ->
        match Worker.Solve_request.of_yojson (Yojson.Safe.from_string request) with
        | Error msg -> Lwt_result.fail (`Capnp (Capnp_rpc.Error.exn "Bad JSON in request: %s" msg))
        | Ok request ->
          Lwt.catch
            (fun () ->
               handle t ~log request
               >|= function [] -> Error `No_solution | x -> Ok x
            )
            (function
              | Failure msg -> Lwt_result.fail (`Msg msg)
              | ex -> Lwt.return_error (`Msg (Fmt.strf "%a" Fmt.exn ex))
            )
          >|= fun selections ->
          let json = Yojson.Safe.to_string (Worker.Solve_response.to_yojson selections) in
          let response, results = Capnp_rpc_lwt.Service.Response.create Results.init_pointer in
          Results.response_set results json;
          Ok response
  end
