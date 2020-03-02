val download_cache : string

val install_project_deps :
  base:string ->
  opam_files:string list ->
  selection:Ocaml_ci_api.Worker.Selection.t ->
  for_user:bool ->
  Dockerfile.t

val dockerfile :
  base:string ->
  opam_files:string list ->
  selection:Ocaml_ci_api.Worker.Selection.t ->
  for_user:bool -> Dockerfile.t
