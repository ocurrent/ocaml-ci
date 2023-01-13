(** Docker execution wrapper. *)

type t = {
  docker_context : string option;
  pool : unit Current.Pool.t;
  build_timeout : Duration.t;
}
(** Describe a docker command to execute. *)

val build :
  t ->
  dockerfile:[ `Contents of string | `File of Fpath.t ] ->
  Current_git.Commit.t ->
  Current_docker.Raw.Image.t Current.Primitive.t
(** Docker [build] with [dockerfile] in a [Commit.t] context. *)

val pull :
  t ->
  arch:Ocaml_version.arch ->
  string ->
  schedule:Current_cache.Schedule.t ->
  Current_docker.Raw.Image.t Current.Primitive.t
(** Docker [pull] an image for [arch] with a scheduled check. *)

val run :
  t ->
  args:string list ->
  Current_docker.Raw.Image.t ->
  unit Current.Primitive.t
(** Docker [run] the image with [args]. *)
