val src : Logs.src

module Log : Logs.LOG

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
[@@deriving show]

val eq_timestamps : timestamps -> timestamps -> bool
(** equality of timestamps - useful for tests *)

val pp_timestamps : Format.formatter -> timestamps -> unit
(** ppx_derived show formatter for timestamps *)

val timestamps_of_job : Current.job_id -> timestamps option
(** Hydrates a timestamps instance for a step/job by looking up timestamps in
    the ocurrent layer. *)

val build_ran_for : (string * timestamps option) list -> float
val first_step_queued_at : timestamps option list -> (float, string) result
