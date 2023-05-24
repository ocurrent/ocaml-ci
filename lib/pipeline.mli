(** Common logic for building a CI pipeline. *)

type build_info = { label : string; variant : Variant.t option }
(** [variant] will be [None] for utility or linting jobs without a platform, and
    [Some v] for jobs building on a particular platform [v]. *)

val build_info_of_spec : Spec.t -> build_info
val build_info_of_label : string -> build_info

val experimental_variant : build_info -> bool
(** Check whether a variant is considered experimental.

    If it is experimental we allow those builds to fail without failing the
    overall build for a commit. *)

val experimental_variant_str : string -> bool
(** Like [experimental_variant], but takes strings for when a [build_info]
    record is unavailable.

    Prefer to use [experimental_variant] when possible, as this function can
    potentially give false positives. *)

val summarise :
  (build_info
  * (([< `Built | `Checked ], [< `Active of 'a | `Msg of string ]) result * 'b))
  list ->
  (unit, [> `Active of [> `Running ] | `Msg of string ]) result
(** Summarise a list of build results.

    NOTE: Failing builds for experimental variants do not fail the build. *)

val get_job_id : 'a Current.t -> string option Current.t
(** Get the job_id associated with ['a Current.t]. *)

val build_with_docker :
  ?ocluster:Cluster_build.t ->
  ?on_cancel:(string -> unit) ->
  repo:Repo_id.t Current.t ->
  analysis:Analyse.Analysis.t Current.t ->
  platforms:Platform.t list Current.t ->
  Current_git.Commit.t Current.t ->
  (build_info * ([> `Built | `Checked ] Current_term.Output.t * string option))
  list
  Current.t
(** [build_with_docker ~repo ~analysis ~platforms commit] creates a suite of
    builds to perform against [commit].

    The builds created will depend on the [platforms] available and the solver
    [analysis] for those platforms. Optionally the builds can be peformed
    locally in docker or on an OCluster instance.

    [on_cancel] is only called when [ocluster] is [Some cluster]. *)
