open Lwt.Infix

let ( >>!= ) = Lwt_result.bind

type t =
  | Remote of Current_ocluster.Connection.t
  | Local of Solver_worker.Solver_request.t Lwt.t

let switch = Current.Switch.create ~label:"solver-remote" ()
let config = Current.Config.v ()

let solve_to_custom req builder =
  let params =
    Yojson.Safe.to_string @@ Ocaml_ci_api.Worker.Solve_request.to_yojson req
  in
  let builder =
    Ocaml_ci_api.Raw.Solve.Builder.Solver.Solve.Params.init_pointer builder
  in
  Ocaml_ci_api.Raw.Solve.Builder.Solver.Solve.Params.request_set builder params

let remote_solve con job request =
  let action =
    Cluster_api.Submission.custom_build
    @@ Cluster_api.Custom.v ~kind:"solve"
    @@ solve_to_custom request
  in
  let build_pool =
    Current_ocluster.Connection.pool ~job ~pool:"solver" ~action ~cache_hint:""
      con
  in
  let dummy_job = Current.Job.create ~label:"solver-job" ~switch ~config () in
  Current.Job.start_with ~pool:build_pool dummy_job ~level:Current.Level.Average
  >>= fun build_job ->
  Capnp_rpc_lwt.Capability.with_ref build_job
    (Current_ocluster.Connection.run_job ~job)

let local () : t =
  Local (Lwt.return (Solver_worker.Solver_request.create ~n_workers:20 ()))

let solve t job request ~log =
  match t with
  | Local ci ->
      ci >>= fun solver ->
      Lwt_switch.with_switch (fun switch ->
          Solver_worker.Solver_request.solve solver ~switch ~request ~log)
  | Remote con -> (
      remote_solve con job request >>!= fun response ->
      match
        Ocaml_ci_api.Worker.Solve_response.of_yojson
          (Yojson.Safe.from_string response)
      with
      | Ok x -> Lwt.return x
      | Error ex -> failwith ex)

let v uri =
  match uri with
  | None -> local ()
  | Some ur ->
      let vat = Capnp_rpc_unix.client_only_vat () in
      let sr = Capnp_rpc_unix.Vat.import_exn vat ur in
      Remote (Current_ocluster.Connection.create sr)

let local_ci t : Solver_worker.Solver_request.t Lwt.t =
  match t with Local ci -> ci | Remote _ -> Fmt.failwith "Not a local solver"
