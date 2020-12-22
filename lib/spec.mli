type ty = [
  | `Opam of [ `Build | `Lint of [ `Doc | `Opam ]] * Selection.t * string list
  | `Opam_fmt of Analyse_ocamlformat.source option
  | `Opam_monorepo of Opam_monorepo.config
] [@@deriving to_yojson, ord]

type t = {
  label : string;
  variant : Variant.t;
  ty : ty;
}

val opam :
  label:string ->
  selection:Selection.t ->
  analysis:Analyse.Analysis.t ->
  [ `Build | `Lint of [ `Doc | `Fmt | `Opam ] ] ->
  t

val opam_monorepo : config:Opam_monorepo.config -> t

val pp : t Fmt.t
val compare : t -> t -> int
val label : t -> string

val pp_summary : ty Fmt.t
