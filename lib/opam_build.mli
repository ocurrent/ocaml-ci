val dockerfile :
  master:Current_git.Commit.t ->
  head:Current_git.Commit.t ->
  base:string ->
  variant:string ->
  revdep:string option ->
  with_tests:bool ->
  pkg:string ->
  for_user:bool ->
  Dockerfile.t
