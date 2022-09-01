module Client = Ocaml_ci_api.Client

let src = Logs.Src.create "ocaml_ci.run_time" ~doc:"ocaml-ci run-time"

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
  | _ ->
      Log.err (fun f ->
          f "[Error] - Timestamp lookup: No entry found for job_id: %s" job_id);
      None
