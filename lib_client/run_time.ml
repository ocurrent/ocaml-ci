module Client = Ocaml_ci_api.Client

let src = Logs.Src.create "ocaml_ci.run_time.lib_client" ~doc:"ocaml-ci lib-client run-time"

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

let timestamps_from_job_info (ji : Client.job_info) :
    (timestamps, string) result =
  match (ji.outcome : Client.State.t) with
  | NotStarted ->
      Option.fold ji.queued_at
        ~none:(Error "Queued_at is None - cannot construct timestamp.")
        ~some:(fun v -> Ok (Queued v))
  | Active ->
      Option.fold ji.queued_at
        ~none:(Error "Queued_at is None - cannot construct timestamp.")
        ~some:(fun q ->
          Option.fold ji.started_at
            ~none:(Error "Started_at is None - cannot construct timestamp.")
            ~some:(fun s : (timestamps, string) result ->
              Ok (Running { queued_at = q; started_at = s })))
  | Passed | Failed _ | Aborted ->
      Option.fold ji.queued_at
        ~none:(Error "Queued_at is None - cannot construct timestamp.")
        ~some:(fun q ->
          Option.fold ji.finished_at
            ~none:(Error "Finished_at is None - cannot construct timestamp.")
            ~some:(fun f : (timestamps, string) result ->
              Ok
                (Finished
                   {
                     queued_at = q;
                     started_at = ji.started_at;
                     finished_at = f;
                   })))
  | Undefined _ -> Error "Outcome is Undefined - cannot construct timestamp."

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
  | _ :: _ -> Error "Multiple analysis steps found"

let total_time = function
  | Cached -> 0.
  | Queued_for v -> v
  | Running v -> v.queued_for +. v.ran_for
  | Finished v -> v.queued_for +. Option.value ~default:0. v.ran_for

let total_of_run_times (build : Client.job_info list) =
  let build_created_at =
    Option.value ~default:0.
      (Option.join @@ Result.to_option @@ build_created_at ~build)
  in
  build
  |> List.filter_map (fun ji -> Result.to_option @@ timestamps_from_job_info ji)
  |> List.map (run_times_from_timestamps ~build_created_at)
  |> List.fold_left (fun subtotal rt -> subtotal +. total_time rt) 0.

let first_step_queued_at (jil : Client.job_info list) =
  if jil = [] then Error "Empty build"
  else
    let minn accum (ji : Client.job_info) =
      Option.fold ~none:accum ~some:(fun v -> min accum v) ji.queued_at
    in
    Ok (List.fold_left minn max_float jil)
