val fmt_spec :
  base:string ->
  ocamlformat_source:Analyse_ocamlformat.source option ->
  Obuilder_spec.stage
(** A build spec that checks the formatting. *)

val doc_spec :
  base:string ->
  opam_files:string list ->
  selection:Selection.t ->
  Obuilder_spec.stage
(** A build spec that checks that the documentation in [./src/] builds without warnings. *)

val opam_lint_spec :
  base:string ->
  opam_files:string list ->
  Obuilder_spec.stage
