val fmt_dockerfile :
  base:string ->
  info:Analyse.Analysis.t ->
  variant:string ->
  for_user:bool ->
  Dockerfile.t
(** A Dockerfile that checks the formatting. *)

val doc_dockerfile :
  base:string ->
  info:Analyse.Analysis.t ->
  variant:string ->
  for_user:bool ->
  Dockerfile.t
(** A Dockerfile that checks that the documentation in [./src/] builds without warnings. *)

