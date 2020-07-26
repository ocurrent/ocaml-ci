val fmt_dockerfile :
  base:string ->
  ocamlformat_source:Analyse_ocamlformat.source option ->
  for_user:bool ->
  Dockerfile.t
(** A Dockerfile that checks the formatting. *)

val doc_dockerfile :
  base:string ->
  opam_files:string list ->
  selection:Selection.t ->
  for_user:bool ->
  Dockerfile.t
(** A Dockerfile that checks that the documentation in [./src/] builds without warnings. *)

val opam_lint_dockerfile :
  base:string ->
  opam_files:string list ->
  for_user:bool ->
  Dockerfile.t
