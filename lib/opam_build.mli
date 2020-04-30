val download_cache : string

val install_project_deps :
  base:string ->
  opam_files:string list ->
  variant:string ->
  for_user:bool ->
  Dockerfile.t

val dockerfile : base:string -> opam_files:string list -> variant:string -> for_user:bool -> Dockerfile.t
