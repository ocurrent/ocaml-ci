type ty = [
  | `Opam of [ `Build | `Lint of [ `Doc | `Opam ]] * Selection.t * string list
  | `Opam_fmt of Analyse_ocamlformat.source option
  | `Duniverse of string list
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

val duniverse : label:string -> variant:Variant.t -> opam_files:string list -> t

val pp : t Fmt.t
val compare : t -> t -> int
val label : t -> string

val pp_summary : ty Fmt.t
