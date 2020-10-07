val download_cache : string

val install_project_deps :
  opam_files:string list ->
  selection:Selection.t ->
  Obuilder_spec.op list

val spec :
  base:string ->
  opam_files:string list ->
  selection:Selection.t ->
  Obuilder_spec.stage
