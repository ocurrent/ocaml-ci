type ty = [
  | `Opam of [ `Build | `Lint of [ `Doc ]] * Ocaml_ci_api.Worker.Selection.t * string list
  | `Opam_fmt of Analyse_ocamlformat.source option
  | `Duniverse
] [@@deriving to_yojson, ord]

type t = {
  label : string;
  variant : string;
  ty : ty;
}

val opam :
  label:string ->
  selection:Ocaml_ci_api.Worker.Selection.t ->
  analysis:Analyse.Analysis.t ->
  [ `Build | `Lint of [ `Doc | `Fmt ] ] ->
  t

val duniverse : label:string -> variant:string -> t

val pp : t Fmt.t
val compare : t -> t -> int
val label : t -> string

val pp_summary : ty Fmt.t
