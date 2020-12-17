val spec :
  base:string ->
  repo:Current_github.Repo_id.t ->
  opam_files:string list ->
  variant:Variant.t ->
  Obuilder_spec.t

val build_cache : Current_github.Repo_id.t -> Obuilder_spec.Cache.t
