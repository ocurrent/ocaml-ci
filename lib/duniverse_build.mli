val spec :
  base:string ->
  repo:Current_github.Repo_id.t ->
  opam_files:string list ->
  variant:Variant.t ->
  Obuilder_spec.stage
