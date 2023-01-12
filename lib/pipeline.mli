(** Check whether a variant is considered experimental.
   If it is experimental we allow those builds to fail without
   failing the overall build for a commit.
 *)
val experimental_variant : string -> bool

val list_errors :
  ok:int -> (string * string) list -> ('a, [> `Msg of string ]) result

val summarise :
  (string *
   (([< `Built | `Checked ], [< `Active of 'a | `Msg of string ]) result * 'b)) list -> 
  (unit, [> `Active of [> `Running ] | `Msg of string ]) result

val get_job_id : 'a Current.t -> string option Current.t

val build_with_docker :
  ?ocluster:Cluster_build.t ->
  repo:Repo_id.t Current.t ->
  analysis:Analyse.Analysis.t Current.t ->
  platforms:Platform.t list Current.t ->
  Current_git.Commit.t Current.t ->
  (string * ([> `Built | `Checked ] Current_term.Output.t * string option))
    list Current.t
