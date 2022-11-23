module Client = Ocaml_ci_api.Client

let src = Logs.Src.create "ocaml_ci.run_time.lib" ~doc:"ocaml-ci lib run-time"

module Log = (val Logs.src_log src : Logs.LOG)

let cmp_floats v1 v2 = abs_float (v1 -. v2) < 0.0000001

type timestamps =
  | Queued of float
  | Running of { queued_at : float; started_at : float }
  | Finished of {
      queued_at : float;
      started_at : float option;
      finished_at : float;
    }
[@@deriving show]

type run_time_info =
  | Cached
    (* This indicates that the job never ran because cached results were used *)
  | Queued_for of float (* elapsed time in seconds *)
  | Running of { queued_for : float; ran_for : float }
  | Finished of { queued_for : float; ran_for : float option }
[@@deriving show]

let eq_timestamps (st1 : timestamps) (st2 : timestamps) =
  match (st1, st2) with
  | Queued v1, Queued v2 -> cmp_floats v1 v2
  | Running v1, Running v2 ->
      cmp_floats v1.queued_at v2.queued_at
      && cmp_floats v1.started_at v2.started_at
  | ( Finished
        {
          queued_at = queued_at1;
          started_at = None;
          finished_at = finished_at1;
        },
      Finished
        {
          queued_at = queued_at2;
          started_at = None;
          finished_at = finished_at2;
        } ) ->
      cmp_floats queued_at1 queued_at2 && cmp_floats finished_at1 finished_at2
  | ( Finished
        {
          queued_at = queued_at1;
          started_at = Some started_at1;
          finished_at = finished_at1;
        },
      Finished
        {
          queued_at = queued_at2;
          started_at = Some started_at2;
          finished_at = finished_at2;
        } ) ->
      cmp_floats queued_at1 queued_at2
      && cmp_floats started_at1 started_at2
      && cmp_floats finished_at1 finished_at2
  | Queued _, (Running _ | Finished _)
  | Running _, (Queued _ | Finished _)
  | ( Finished { started_at = None; _ },
      (Queued _ | Running _ | Finished { started_at = Some _; _ }) )
  | ( Finished { started_at = Some _; _ },
      (Queued _ | Running _ | Finished { started_at = None; _ }) ) ->
      false

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
      | Some started_at ->
          Some (Running { queued_at = ready; started_at })
          (* The job is running so we work off the start time in the Job map *)
      | None -> (
          match running with
          | None -> Some (Queued ready)
          | Some _ ->
              Some
                (Finished
                   {
                     queued_at = ready;
                     started_at = running;
                     finished_at = finished;
                   })))
  | [] -> (
      (* No db entry for the job. Check Current.Job.jobs map *)
      match timestamp_from_job_map with
      | Some started_at -> Some (Running { queued_at = started_at; started_at })
      | None ->
          Log.info (fun f ->
              f "[Info] - Timestamp lookup: No entry found for job_id: %s"
                job_id);
          None)
  | x ->
      List.iter
        (fun y ->
          Log.err (fun f ->
              f "[Error] - Unhandled situation in timestamp lookup: %s"
                y.Current_cache.Db.job_id))
        x;
      None

let queued_at = function
  | Queued v -> v
  | Running v -> v.queued_at
  | Finished v -> v.queued_at

let partition_build_steps build =
  let analysis_steps, rest =
    List.partition (fun (variant, _) -> String.equal variant "(analysis)") build
  in
  let analysis_steps = List.map snd analysis_steps in
  let rest = List.map snd rest in
  match analysis_steps with
  | [] -> Error "No analysis step found"
  | [ h ] -> Ok (h, rest)
  | _ :: _ -> Error "Multiple analysis steps found"

let run_times_from_timestamps ~build_created_at
    ?(current_time = Unix.gettimeofday ()) (ts : timestamps) : run_time_info =
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
              { queued_for = v -. queued_at; ran_for = Some (finished_at -. v) }
      )

let total_time = function
  | Cached -> 0.
  | Queued_for v -> v
  | Running v -> v.queued_for +. v.ran_for
  | Finished v -> v.queued_for +. Option.value ~default:0. v.ran_for

let max_of_step_run_times ~build_created_at ts =
  let sorted_steps =
    ts
    |> List.map (fun ts ->
           total_time @@ run_times_from_timestamps ~build_created_at ts)
    |> List.sort Float.compare
    |> List.rev
  in
  Option.value ~default:0. (List.nth_opt sorted_steps 0)

let build_ran_for build =
  let partitioned = Result.to_option @@ partition_build_steps build in
  match partitioned with
  | None -> 0.
  | Some (analysis_step, rest) -> (
      match analysis_step with
      | None -> 0.
      | Some analysis_step_timestamps ->
          let build_created_at = queued_at analysis_step_timestamps in
          let run_time_analysis_step =
            total_time
            @@ run_times_from_timestamps ~build_created_at
                 analysis_step_timestamps
          in
          let rest = List.filter_map Fun.id rest in
          let build_ran_for =
            run_time_analysis_step
            +. max_of_step_run_times ~build_created_at rest
          in
          build_ran_for)

let first_step_queued_at ts =
  (* for_all holds for the empty list as well as preventing transient
      error when only the analysis step exists with a None timestamp.
      Error would be caused by trying to format [max_float] as a string. *)
  if List.for_all Option.is_none ts then Error "Empty build"
  else
    let minn accum ts =
      Option.fold ~none:accum ~some:(fun v -> min accum (queued_at v)) ts
    in
    Ok (List.fold_left minn max_float ts)
