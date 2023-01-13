(** Common logic for building a CI pipeline. *)

val experimental_variant : string -> bool
(** Check whether a variant is considered experimental.

    If it is experimental we allow those builds to fail without failing the
    overall build for a commit. *)

val summarise :
  (string
  * (([< `Built | `Checked ], [< `Active of 'a | `Msg of string ]) result * 'b))
  list ->
  (unit, [> `Active of [> `Running ] | `Msg of string ]) result
(** Summarise a list of build results.

    NOTE: Failing builds for experimental variants do not fail the build. *)

val get_job_id : 'a Current.t -> string option Current.t
(** Get the job_id associated with ['a Current.t]. *)

val build_with_docker :
  ?ocluster:Cluster_build.t ->
  repo:Repo_id.t Current.t ->
  analysis:Analyse.Analysis.t Current.t ->
  platforms:Platform.t list Current.t ->
  Current_git.Commit.t Current.t ->
  (string * ([> `Built | `Checked ] Current_term.Output.t * string option)) list
  Current.t
(** [build_with_docker ~repo ~analysis ~platforms commit] creates a suite of
    builds to perform against [commit].

    The builds created will depend on the [platforms] available and the solver
    [analysis] for those platforms. Optionally the builds can be peformed
    locally in docker or on an OCluster instance. *)
