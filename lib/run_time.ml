let src = Logs.Src.create "ocaml_ci.run_time.lib" ~doc:"ocaml-ci lib run-time"

module Log = (val Logs.src_log src : Logs.LOG)
module Client = Ocaml_ci_api.Client

let cmp_floats v1 v2 = abs_float (v1 -. v2) < 0.0000001

module Duration = struct
  let pp ppf t =
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
      let sec = to_sec t in
      Fmt.pf ppf "%ds" sec

  let pp_opt =
    Option.fold ~none:"-" ~some:(fun v -> Fmt.str "%a" pp (Duration.of_f v))

  let to_ts_string (tt : float) =
    let ts = Timedesc.of_timestamp_float_s tt in
    Timedesc.to_string
      ~format:
        "{mon:Xxx} {day:0X} {hour:0X}:{min:0X} \
         {tzoff-sign}{tzoff-hour:0X}:{tzoff-min:0X}"
    @@ Option.get ts

  let pp_readable_opt v = Option.fold ~none:"-" ~some:to_ts_string v
end

module Timestamp = struct
  type t =
    | Queued of float
    | Running of { queued_at : float; started_at : float }
    | Finished of {
        queued_at : float;
        started_at : float option;
        finished_at : float;
      }
  [@@deriving show]

  let eq (st1 : t) (st2 : t) =
    match (st1, st2) with
    | Queued v1, Queued v2 -> cmp_floats v1 v2
    | Running v1, Running v2 ->
        cmp_floats v1.queued_at v2.queued_at
        && cmp_floats v1.started_at v2.started_at
    | ( Finished ({ started_at = None; _ } as v1),
        Finished ({ started_at = None; _ } as v2) ) ->
        cmp_floats v1.queued_at v2.queued_at
        && cmp_floats v1.finished_at v2.finished_at
    | ( Finished ({ started_at = Some _; _ } as v1),
        Finished ({ started_at = Some _; _ } as v2) ) ->
        cmp_floats v1.queued_at v2.queued_at
        && cmp_floats (Option.get v1.started_at) (Option.get v2.started_at)
        && cmp_floats v1.finished_at v2.finished_at
    | Queued _, (Running _ | Finished _)
    | Running _, (Queued _ | Finished _)
    | ( Finished { started_at = None; _ },
        (Queued _ | Running _ | Finished { started_at = Some _; _ }) )
    | ( Finished { started_at = Some _; _ },
        (Queued _ | Running _ | Finished { started_at = None; _ }) ) ->
        false

  let of_job_id_opt job_id =
    let timestamp_from_job_map =
      Option.bind (Current.Job.lookup_running job_id) (fun job ->
          match Lwt.state (Current.Job.start_time job) with
          | Lwt.Sleep | Lwt.Fail _ -> None
          | Lwt.Return t -> Some t)
    in
    let job_db_entry = Current_cache.Db.query ~job_prefix:job_id () in
    match job_db_entry with
    | [ { Current_cache.Db.ready; running; finished; _ } ] -> (
        match timestamp_from_job_map with
        | Some started_at ->
            Some (Running { queued_at = ready; started_at })
            (* The job is running so we work off the start time in the Job map *)
        | None ->
            Option.fold running ~none:(Some (Queued ready)) ~some:(fun _ ->
                Some
                  (Finished
                     {
                       queued_at = ready;
                       started_at = running;
                       finished_at = finished;
                     })))
    | [] ->
        ((* No db entry for the job. Check Current.Job.jobs map *)
         Option.map (fun started_at ->
             Running { queued_at = started_at; started_at }))
          timestamp_from_job_map
    | x ->
        List.iter
          (fun y ->
            Log.err (fun f ->
                f "[Error] - Unhandled situation in timestamp lookup: %s"
                  y.Current_cache.Db.job_id))
          x;
        None

  let of_job_info (ji : Client.job_info) : (t, string) result =
    match (ji.outcome : Client.State.t) with
    | NotStarted ->
        Option.fold ji.queued_at
          ~none:(Error "Queued_at is None - cannot construct timestamp.")
          ~some:(fun v -> Ok (Queued v))
    | Active ->
        Option.fold ji.queued_at
          ~none:(Error "Queued_at is None - cannot construct timestamp.")
          ~some:(fun queued_at ->
            Option.fold ji.started_at
              ~none:(Error "Started_at is None - cannot construct timestamp.")
              ~some:(fun started_at : (t, string) result ->
                Ok (Running { queued_at; started_at })))
    | Passed | Failed _ | Aborted ->
        Option.fold ji.queued_at
          ~none:(Error "Queued_at is None - cannot construct timestamp.")
          ~some:(fun queued_at ->
            Option.fold ji.finished_at
              ~none:(Error "Finished_at is None - cannot construct timestamp.")
              ~some:(fun finished_at ->
                Ok
                  (Finished
                     { queued_at; started_at = ji.started_at; finished_at })))
    | Undefined _ -> Error "Outcome is Undefined - cannot construct timestamp."

  let queued_at = function
    | Queued v -> v
    | Running v -> v.queued_at
    | Finished v -> v.queued_at
end

module TimeInfo = struct
  type t =
    | Cached
      (* This indicates that the job never ran because cached results were used *)
    | Queued_for of float (* elapsed time in seconds *)
    | Running of { queued_for : float; ran_for : float }
    | Finished of { queued_for : float; ran_for : float option }
  [@@deriving show]

  let total = function
    | Cached -> 0.
    | Queued_for v -> v
    | Running v -> v.queued_for +. v.ran_for
    | Finished v -> v.queued_for +. Option.value ~default:0. v.ran_for

  let queued_for = function
    | Cached -> 0.
    | Queued_for v -> v
    | Running v -> v.queued_for
    | Finished v -> v.queued_for

  let ran_for = function
    | Cached -> 0.
    | Queued_for _ -> 0.
    | Running v -> v.ran_for
    | Finished v -> Option.value ~default:0. v.ran_for

  let of_timestamp ~build_created_at ?(current_time = Unix.gettimeofday ())
      (ts : Timestamp.t) : t =
    match ts with
    | Queued v -> Queued_for (current_time -. v)
    | Running { queued_at; started_at } ->
        Running
          {
            queued_for = started_at -. queued_at;
            ran_for = current_time -. started_at;
          }
    | Finished { queued_at; started_at; finished_at } -> (
        if finished_at < build_created_at then Cached
        else
          match started_at with
          | None ->
              Finished { queued_for = finished_at -. queued_at; ran_for = None }
          | Some v ->
              Finished
                {
                  queued_for = v -. queued_at;
                  ran_for = Some (finished_at -. v);
                })
end

module TimeList = struct
  let partition_build_steps build =
    let analysis_steps, rest =
      List.partition
        (fun (variant, _) -> String.equal variant "(analysis)")
        build
    in
    let analysis_steps = List.map snd analysis_steps in
    let rest = List.map snd rest in
    match analysis_steps with
    | [] -> Error "No analysis step found"
    | [ h ] -> Ok (h, rest)
    | _ -> Error "Multiple analysis steps found"

  let max_of_step_run_times ~build_created_at ts =
    let sorted_steps =
      ts
      |> List.map (fun ts ->
             TimeInfo.total @@ TimeInfo.of_timestamp ~build_created_at ts)
      |> List.sort (fun x y -> Float.compare x y |> Int.neg)
    in
    Option.value ~default:0. (List.nth_opt sorted_steps 0)

  let build_ran_for ts =
    let partitioned = Result.to_option @@ partition_build_steps ts in
    match partitioned with
    | None | Some (None, _) -> 0.
    | Some (Some analysis_step_timestamps, rest) ->
        let build_created_at = Timestamp.queued_at analysis_step_timestamps in
        let run_time_analysis_step =
          TimeInfo.of_timestamp ~build_created_at analysis_step_timestamps
          |> TimeInfo.total
        in
        let rest = List.filter_map Fun.id rest in
        let build_ran_for =
          run_time_analysis_step +. max_of_step_run_times ~build_created_at rest
        in
        build_ran_for

  let first_step_queued_at ts =
    (* for_all holds for the empty list as well as preventing transient
        error when only the analysis step exists with a None timestamp.
        Error would be caused by trying to format [max_float] as a string. *)
    match ts with
    | [] -> Error "Empty build"
    | _ ->
        Ok
          (List.fold_left
             (fun acc v -> min acc (Timestamp.queued_at v))
             max_float ts)

  let total_of_run_times ~build_created_at ts =
    ts
    |> List.map (TimeInfo.of_timestamp ~build_created_at)
    |> List.fold_left (fun subtotal rt -> subtotal +. TimeInfo.total rt) 0.
end

module Job = struct
  let partition_build_steps build =
    let analysis (ji : Client.job_info) =
      let re = Str.regexp_string "analysis" in
      try
        ignore (Str.search_forward re ji.variant 0);
        true
      with Not_found -> false
    in
    let analysis_steps, rest = List.partition analysis build in
    match analysis_steps with
    | [] -> Error "No analysis step found"
    | [ h ] -> Ok (h, rest)
    | _ :: _ -> Error "Multiple analysis steps found"

  let build_created_at ~build =
    let analysis (ji : Client.job_info) =
      let re = Str.regexp_string "analysis" in
      try
        ignore (Str.search_forward re ji.variant 0);
        true
      with Not_found -> false
    in
    let filtered = List.filter analysis build in
    match filtered with
    | [] -> Error "No analysis step found"
    | [ h ] -> Ok h.queued_at
    | _ -> Error "Multiple analysis steps found"

  let total_of_run_times (build : Client.job_info list) =
    let build_created_at =
      Option.value ~default:0.
        (Option.join @@ Result.to_option @@ build_created_at ~build)
    in
    let f job_info =
      match Timestamp.of_job_info job_info with
      | Error _ -> None
      | Ok timestamp ->
          TimeInfo.of_timestamp ~build_created_at timestamp |> Option.some
    in
    List.filter_map f build
    |> List.fold_left (fun subtotal rt -> subtotal +. TimeInfo.total rt) 0.

  let max_of_step_run_times ~build_created_at steps =
    let f job_info =
      match Timestamp.of_job_info job_info with
      | Error _ -> None
      | Ok timestamp ->
          TimeInfo.of_timestamp ~build_created_at timestamp
          |> TimeInfo.total
          |> Option.some
    in
    let sorted_steps =
      List.filter_map f steps
      |> List.sort (fun x y -> Float.compare x y |> Int.neg)
    in
    Option.value ~default:0. (List.nth_opt sorted_steps 0)

  let build_run_time build =
    let partitioned = Result.to_option @@ partition_build_steps build in
    match partitioned with
    | None -> 0.
    | Some (analysis_step, rest) -> (
        match Timestamp.of_job_info analysis_step with
        | Error _ -> 0.
        | Ok analysis_step_timestamps ->
            let build_created_at =
              Option.value ~default:0. analysis_step.queued_at
            in
            let run_time_analysis_step =
              TimeInfo.total
              @@ TimeInfo.of_timestamp ~build_created_at
                   analysis_step_timestamps
            in
            run_time_analysis_step
            +. max_of_step_run_times ~build_created_at rest)

  let first_step_queued_at (jil : Client.job_info list) =
    (* for_all holds for the empty list as well as preventing transient
       error when only the analysis step exists with a None timestamp.
       Error would be caused by trying to format [max_float] as a string. *)
    let no_queued_at_ts (ji : Client.job_info) = ji.queued_at = None in
    if List.for_all no_queued_at_ts jil then Error "Empty build"
    else
      let min acc (ji : Client.job_info) =
        Option.fold ~none:acc ~some:(fun v -> min acc v) ji.queued_at
      in
      Ok (List.fold_left min max_float jil)
end
