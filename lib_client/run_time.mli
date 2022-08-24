val src : Logs.src

module Log : Logs.LOG

val duration_pp : Format.formatter -> int64 -> unit

type timestamps =
  | Queued of float
  | Running of { queued_at : float; started_at : float }
  | Finished of {
      queued_at : float;
      started_at : float option;
      finished_at : float;
    }
      (** Type to encode the timestamps of a step. All floats represent Unix
          epoch time - i.e. seconds from 1970-01-01 00:00:00 GMT

          The step is either:

          - Cached - a previously run instance was used and the step did not run
          - Queued - the associated timestamp indicates when it was ready to run
            (enqueued)
          - Running - with timestamps for when it was ready and when it started
          - Finished - with timestamps for when it was ready, when it started
            and when it finished. Note that it may never run -- it may be
            cancelled or it may timeout. *)

type run_time_info =
  | Cached
  | Queued_for of float
  | Running of { queued_for : float; ran_for : float }
  | Finished of { queued_for : float; ran_for : float option }
      (** Type to encode durations. All floats represent seconds.

          The step is either:

          - Cached - a previously run instance of the step was used and nothing
            was run
          - Queued - the associated duration represents the number of seconds it
            has been queued for (relative to now)
          - Running - has a queued_for duration, and a ran_for duration. The
            latter should be interpreted as the number of seconds that it has
            been running for
          - Finished - has a queued_for and an optional ran_for duration. The
            ran_for duration will be None when it did not run. *)

val queued_for : run_time_info -> float
val ran_for : run_time_info -> float

val eq_timestamps : timestamps -> timestamps -> bool
(** equality of timestamps - useful for tests *)

val pp_timestamps : Format.formatter -> timestamps -> unit
(** ppx_derived show formatter for timestamps *)

val pp_run_time_info : Format.formatter -> run_time_info -> unit
(** ppx_derived show formatter for run_time_info *)

val run_times_from_timestamps :
  build_created_at:float -> ?current_time:float -> timestamps -> run_time_info
(** Calculates the run_time of a step from timestamps and relative to the time
    at which the build was created. Takes an optional current_time that defaults
    to Unix.gettimeofday. *)

val timestamps_from_job_info :
  Ocaml_ci_api.Client.job_info -> (timestamps, string) result
(** Derives timestamps from an instance of job_info *)

val first_step_queued_at : Ocaml_ci_api.Client.job_info list -> float
(** Derives timestamps from a list of job_info *)

val build_created_at :
  build:Ocaml_ci_api.Client.job_info list -> (float option, string) result
(** Takes a list of job_info which is meant to be the steps in a build. Returns
    the queued_at timestamp of the analysis step or None if there is not exactly
    one analysis step in the list. *)

val total_of_run_times : Ocaml_ci_api.Client.job_info list -> float
