let src = Logs.Src.create "ocaml_ci.run_time" ~doc:"ocaml-ci run-time"

module Log = (val Logs.src_log src : Logs.LOG)

let duration_pp ppf t =
  let hour = 3600_000_000_000L in
  let day = Int64.mul 24L hour in
  let year = Int64.mul 8766L hour in
  let open Duration in
  let min = to_min t in
  if min > 0 then
    let y = to_year t in
    let left = Int64.rem t year in
    let d = to_day left in
    let left = Int64.rem left day in
    if y > 0 then Fmt.pf ppf "%da%dd" y d
    else
      let h = to_hour left in
      let left = Int64.rem left hour in
      if d > 0 then Fmt.pf ppf "%dd%02dh" d h
      else
        let min = to_min left in
        let left = Int64.sub t (of_min min) in
        let sec = to_sec left in
        if h > 0 then Fmt.pf ppf "%dh%02dm" h min
        else (* if m > 0 then *)
          Fmt.pf ppf "%dm%02ds" min sec
  else
    (* below one minute *)
    let fields t =
      let sec = to_sec_64 t in
      let left = Int64.sub t (of_sec_64 sec) in
      let ms = to_ms_64 left in
      let left = Int64.sub left (of_ms_64 ms) in
      let us = to_us_64 left in
      let ns = Int64.(sub left (of_us_64 us)) in
      (sec, ms, us, ns)
    in
    let s, ms, us, ns = fields t in
    if s > 0L then Fmt.pf ppf "%Lds" s
    else if ms > 0L then Fmt.pf ppf "%Ldms" ms
    else (* if us > 0 then *)
      Fmt.pf ppf "%Ld.%03Ldus" us ns

let cmp_floats v1 v2 = abs_float(v1 -. v2) < 0.0000001

type timestamps =
  | Queued of float (* timestamp -- step is ready and queued *)
  | Running of { ready : float; started : float }
  | Finished of { ready : float; started : float option; finished : float }
[@@deriving show]

type run_time_info =
  | Cached
    (* This indicates that the job never ran because cached results were used *)
  | Queued_for of float (* elapsed time in milliseconds *)
  | Running of { queued_for : float; ran_for : float }
  | Finished of { queued_for : float; ran_for : float option }
[@@deriving show]

let eq_timestamps st1 st2 =
    match (st1, st2) with
    | Queued v1, Queued v2 -> cmp_floats v1 v2
    | Running v1, Running v2 -> cmp_floats v1.ready v2.ready && cmp_floats v1.started v2.started
    | Finished {ready=ready1; started=None; finished=finished1}, Finished {ready=ready2; started=None; finished=finished2} ->
        cmp_floats ready1 ready2 && cmp_floats finished1 finished2
    | Finished {ready=ready1; started=Some started1; finished=finished1}, Finished {ready=ready2; started=Some started2; finished=finished2} ->
        cmp_floats ready1 ready2 && cmp_floats started1 started2 && cmp_floats finished1 finished2
    | Queued _, (Running _ | Finished _ )
    | Running _, (Queued _ | Finished _) 
    | Finished {started=None; _}, (Queued _ | Running _ | Finished {started=Some _; _})
    | Finished {started=Some _; _}, (Queued _ | Running _ | Finished {started=None; _}) -> false

let info_to_string = function
  | Cached -> " (cached)"
  | Queued_for v -> Fmt.str " (%a queued)" duration_pp (Duration.of_f v)
  | Running v ->
      Fmt.str " (%a queued) (running for %a)" duration_pp
        (Duration.of_f v.queued_for)
        duration_pp (Duration.of_f v.ran_for)
  | Finished { queued_for; ran_for = None } ->
      Fmt.str "  (%a queued) (ran for -)" duration_pp (Duration.of_f queued_for)
  | Finished { queued_for; ran_for = Some ran_for' } ->
      Fmt.str "  (%a queued) (ran for %a)" duration_pp
        (Duration.of_f queued_for) duration_pp (Duration.of_f ran_for')

let timestamps_of_job job_id : timestamps option =
  let timestamp_from_job_map =
    match Current.Job.lookup_running job_id with
    | Some job -> (
        match Lwt.state (Current.Job.start_time job) with
        | Lwt.Sleep | Lwt.Fail _ -> None
        | Lwt.Return t -> Some t)
    | None -> None
  in
  let job_db_entry = Current_cache.Db.query ~job_prefix:job_id () in
  match job_db_entry with
  | [ { Current_cache.Db.ready; running; finished; _ } ] -> (
      match timestamp_from_job_map with
      | Some started ->
          Some (Running { ready; started })
          (* The job is running so we work off the start time in the Job map *)
      | None -> (
          match running with
          | None -> Some (Queued ready)
          | Some _ -> Some (Finished { ready; started = running; finished })))
  | _ ->
      Log.err (fun f ->
          f "[Error] - Timestamp lookup: No entry found for job_id: %s" job_id);
      None

let info_from_timestamps ?(current_time = Unix.gettimeofday ())
    (ts : timestamps) : run_time_info =
  match ts with
  | Queued v -> Queued_for (current_time -. v)
  | Running { ready; started } ->
      Running
        { queued_for = started -. ready; ran_for = current_time -. started }
  | Finished { ready; started; finished } -> (
      if finished < current_time then Cached
      else
        match started with
        | None -> Finished { queued_for = finished -. ready; ran_for = None }
        | Some v ->
            Finished { queued_for = v -. ready; ran_for = Some (finished -. v) }
      )

let merge (st1 : timestamps) (st2 : timestamps) =
  match st1 with
  | Queued v1 -> (
      match st2 with
      | Queued v2 -> Queued (Float.min v1 v2)
      | Running { ready; started } ->
          Running { ready = Float.min v1 ready; started }
      | Finished v2 -> Queued (Float.min v1 v2.ready))
  | Running v1 -> (
      match st2 with
      | Queued v2 ->
          Running { ready = Float.min v1.ready v2; started = v1.started }
      | Running v2 ->
          Running
            {
              ready = Float.min v1.ready v2.ready;
              started = Float.min v1.started v2.started;
            }
      | Finished v2 -> (
          match v2.started with
          | None ->
              Running
                { ready = Float.min v1.ready v2.ready; started = v1.started }
          | Some started' ->
              Running
                {
                  ready = Float.min v1.ready v2.ready;
                  started = Float.min v1.started started';
                }))
  | Finished { ready; started = None; finished } -> (
      match st2 with
      | Queued v2 -> Queued (Float.min ready v2)
      | Running v2 ->
          Running { ready = Float.min ready v2.ready; started = v2.started }
      | Finished v2 ->
          Finished
            {
              ready = Float.min ready v2.ready;
              started = None;
              finished = Float.max finished v2.finished;
            })
  | Finished { ready; started = Some started; finished } -> (
      match st2 with
      | Queued v2 -> Queued (Float.min ready v2)
      | Running v2 ->
          Running
            {
              ready = Float.min ready v2.ready;
              started = Float.min started v2.started;
            }
      | Finished v2 -> (
          match v2.started with
          | None ->
              Finished
                {
                  ready = Float.min ready v2.ready;
                  started = None;
                  finished = Float.max finished v2.finished;
                }
          | Some started' ->
              Finished
                {
                  ready = Float.min ready v2.ready;
                  started = Some (Float.min started started');
                  finished = Float.max finished v2.finished;
                }))

let merge' (st1 : timestamps option) (st2 : timestamps option) =
  match (st1, st2) with
  | None, None -> None
  | Some st1, None -> Some st1
  | None, Some st2 -> Some st2
  | Some st1, Some st2 -> Some (merge st1 st2)

let of_build build =
  (* TODO: The below is useful for print debugging - cleanup and remove once stable. *)
  (* let mtimestamps = Index.get_job_ids ~owner ~name ~hash |> List.map timestamps_of_job in *)
  (* List.iter (fun t -> Option.get(t) |> Fmt.str "%a" pp_timestamps |> print_string) mtimestamps; *)
  (* Index.get_job_ids ~owner ~name ~hash *)
  build
  |> List.map timestamps_of_job
  |> List.fold_left merge' None
