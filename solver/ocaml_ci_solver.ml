module Worker = Ocaml_ci_api.Worker

let run_toplevel () =
  match Worker.Solve_request.of_yojson (Yojson.Safe.from_channel stdin) with
  | Error msg -> Fmt.failwith "Bad request: %s" msg
  | Ok request ->
    let response =
      try Ok (Solver.solve request)
      with
      | Failure msg -> Error (`Msg msg)
      | ex -> Fmt.error_msg "%a" Fmt.exn ex
    in
    Yojson.Safe.to_channel stdout (Worker.Solve_response.to_yojson response)

let main ~self args =
  Logs.(set_level (Some Info));
  Logs.set_reporter Log.reporter;
  match args with
  | [] -> run_toplevel ()
  | _ -> Fmt.failwith "Usage: %a" Fmt.(list string) self
