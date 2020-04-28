val download_cache : string

val install_project_deps :
  base:string ->
  info:Analyse.Analysis.t ->
  variant:string ->
  for_user:bool ->
  Dockerfile.t

val dockerfile : base:string -> info:Analyse.Analysis.t -> variant:string -> for_user:bool -> Dockerfile.t
