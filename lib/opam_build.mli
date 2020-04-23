val dockerfile :
  for_user:bool ->
  base:string ->
  variant:string ->
  revdep:string option ->
  with_tests:bool ->
  pkg:string ->
  Dockerfile.t
