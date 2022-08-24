val src : Logs.src

module Log : Logs.LOG

type timestamps =
  | Queued of float
  | Running of { ready : float; started : float }
  | Finished of { ready : float; started : float option; finished : float }
      (** Type to encode the timestamps of a step or a build. All floats
          represent Unix epoch time - i.e. milliseconds from 1970-01-01 00:00:00
          GMT

          The step or build is either:

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
      (** Type to encode the durations of the various stages of a step or build.
          All floats represent milliseconds.

          The step or build is either:

          - Cached - a previously run instance of the step was used and nothing
            was run
          - Queued - the associated duration represents the number of
            milliseconds it has been queued for (relative to now)
          - Running - has a queued_for duration, and a ran_for duration. The
            latter should be interpreted as the number of milliseconds that it
            has been running for
          - Finished - has a queued_for and an optional ran_for duration. The
            ran_for duration will be None when it did not run. *)

val timestamps_eq : timestamps -> timestamps -> bool
(** equality of timestamps - useful for tests *)

val pp_timestamps : Format.formatter -> timestamps -> unit
(** ppx_derived show formatter for timestamps *)

val pp_run_time_info : Format.formatter -> run_time_info -> unit
(** ppx_derived show formatter for run_time_info *)

val info_to_string : run_time_info -> string
(** Utility function to present run_time_info *)

val timestamps_of_job : Current.job_id -> timestamps option
(** Hydrates a timestamps instance for a step/job by looking up timestamps in
    the ocurrent layer. *)

val info_from_timestamps : ?current_time:float -> timestamps -> run_time_info
(** Calculates durations from timestamps. Takes an optional `current_time` that
    defaults to Unix.gettimeofday *)

val merge : timestamps -> timestamps -> timestamps
(** Merges two timestamps describe the timings of the 'build' that is
    represented by the two steps that correspond to the given timestamps. *)

val of_build : string list -> timestamps option
(** Derives the timestamps of a build specified by `[ job_id ]` *)
