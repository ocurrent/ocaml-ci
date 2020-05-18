module Worker = Ocaml_ci_api.Worker

let pp_timestamp f x =
  let open Unix in
  let tm = gmtime x in
  Fmt.pf f "%04d-%02d-%02d %02d:%02d.%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let reporter =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stderr ("%a %14s %a @[" ^^ fmt ^^ "@]@.")
      pp_timestamp (Unix.gettimeofday ())
      src
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

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
  Logs.set_reporter reporter;
  match args with
  | [] -> run_toplevel ()
  | _ -> Fmt.failwith "Usage: %a" Fmt.(list string) self
