(** Runtime calculations for builds. *)

val src : Logs.src

module Log : Logs.LOG

module Timestamp : sig
  type t =
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

            - Cached - a previously run instance was used and the step did not
              run
            - Queued - the associated timestamp indicates when it was ready to
              run (enqueued)
            - Running - with timestamps for when it was ready and when it
              started
            - Finished - with timestamps for when it was ready, when it started
              and when it finished. Note that it may never run -- it may be
              cancelled or it may timeout. *)

  val eq : t -> t -> bool
  (** equality of timestamps - useful for tests *)

  val pp : Format.formatter -> t -> unit
  (** ppx_derived show formatter for timestamps *)

  val of_job_id_opt : Current.job_id -> t option
  (** Hydrates a timestamps instance for a step/job by looking up timestamps in
      the ocurrent layer. *)

  val of_job_info : Ocaml_ci_api.Client.job_info -> (t, string) result
  (** Derives timestamps from an instance of job_info *)
end

module TimeInfo : sig
  type t =
    | Cached
    | Queued_for of float
    | Running of { queued_for : float; ran_for : float }
    | Finished of { queued_for : float; ran_for : float option }
  [@@deriving show]

  val queued_for : t -> float
  val ran_for : t -> float

  val of_timestamp :
    build_created_at:float -> ?current_time:float -> Timestamp.t -> t
  (** Calculates the run_time of a step from timestamps and relative to the time
      at which the build was created. Takes an optional current_time that
      defaults to Unix.gettimeofday. *)
end

module TimeList : sig
  val build_ran_for : (string * Timestamp.t option) list -> float
  val first_step_queued_at : Timestamp.t list -> (float, string) result
  val total_of_run_times : build_created_at:float -> Timestamp.t list -> float
end

module TimeClient : sig
  module Duration : sig
    val pp : Format.formatter -> int64 -> unit
  end

  module Timestamp : sig
    type t =
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

              - Queued - the associated timestamp indicates when it was ready to
                run (enqueued)
              - Running - with timestamps for when it was ready and when it
                started
              - Finished - with timestamps for when it was ready, when it
                started and when it finished. Note that it may never run -- it
                may be cancelled or it may timeout. *)

    val pp : Format.formatter -> t -> unit
    (** ppx_derived show formatter for timestamps *)

    val eq : t -> t -> bool
    (** equality of timestamps - useful for tests *)
  end

  module TimeInfo : sig
    type t =
      | Cached
      | Queued_for of float
      | Running of { queued_for : float; ran_for : float }
      | Finished of { queued_for : float; ran_for : float option }
          (** Type to encode durations. All floats represent seconds.

              The step is either:

              - Cached - a previously run instance of the step was used and
                nothing was run
              - Queued - the associated duration represents the number of
                seconds it has been queued for (relative to now)
              - Running - has a queued_for duration, and a ran_for duration. The
                latter should be interpreted as the number of seconds that it
                has been running for
              - Finished - has a queued_for and an optional ran_for duration.
                The ran_for duration will be None when it did not run. *)

    val pp : Format.formatter -> t -> unit
    (** ppx_derived show formatter for run_time_info *)

    val queued_for : t -> float
    val ran_for : t -> float

    val total : t -> float
    (** The total time taken -- the sum of the queued and running times *)

    val of_timestamp :
      build_created_at:float -> ?current_time:float -> Timestamp.t -> t
    (** Calculates the run_time of a step from timestamps and relative to the
        time at which the build was created. Takes an optional current_time that
        defaults to Unix.gettimeofday. *)
  end
end

module Job : sig
  val first_step_queued_at :
    Ocaml_ci_api.Client.job_info list -> (float, string) result
  (** Derives timestamps from a list of job_info *)

  val build_created_at :
    build:Ocaml_ci_api.Client.job_info list -> (float option, string) result
  (** Takes a list of job_info which is meant to be the steps in a build.
      Returns the queued_at timestamp of the analysis step or None if there is
      not exactly one analysis step in the list. *)

  val max_of_step_run_times :
    build_created_at:float -> Ocaml_ci_api.Client.job_info list -> float
  (** Takes a list of steps and returns the maximum of the run-times of the
      steps. This is done by calculating the run-times of the steps in the list,
      sorting the resulting list of run-times, and returning the head of the
      list (defaulting to 0) *)

  val total_of_run_times : Ocaml_ci_api.Client.job_info list -> float
  (** Takes a list of job_info which is meant to be the steps in a build.
      Returns total of run times of the steps in the build.*)

  val build_run_time : Ocaml_ci_api.Client.job_info list -> float
  (** Takes a list of job_info which is meant to be the steps in a build.
      Returns the (run-time of the analysis step) + max(run-times of the steps
      in the build) *)
end
