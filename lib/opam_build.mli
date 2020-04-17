val dockerfile :
  base:string ->
  variant:string ->
  revdep:string option ->
  with_tests:bool ->
  pkg:string ->
  Dockerfile.t
