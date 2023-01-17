(** Generate obuilder specs for building opam packages with opam and dune. *)

val download_cache : string
(** Location of the download cache for opam. *)

val install_project_deps :
  opam_version:Opam_version.t ->
  opam_files:string list ->
  selection:Selection.t ->
  Obuilder_spec.op list
(** Create an obuilder operations spec that describe how to install project
    dependencies for [opam_files] using opam and dune. *)

val spec :
  base:string ->
  opam_version:Opam_version.t ->
  opam_files:string list ->
  selection:Selection.t ->
  Obuilder_spec.t
(** Create an obuilder spec that builds [opam_files] using opam and dune. *)
