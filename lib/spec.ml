type opam_files = string list [@@deriving to_yojson, ord]

type ty = [
  | `Opam of [ `Build | `Lint of [ `Doc | `Opam ]] * Selection.t * opam_files
  | `Opam_fmt of Selection.t * Analyse_ocamlformat.source option
  | `Opam_monorepo of Opam_monorepo.config
] [@@deriving to_yojson, ord]

type t = {
  label : string;
  variant : Variant.t;
  ty : ty;
} [@@deriving ord]

let opam ~label ~selection ~analysis op =
  let variant = selection.Selection.variant in
  let ty =
    match op with
    | `Build | `Lint (`Doc | `Opam) as x -> `Opam (x, selection, Analyse.Analysis.opam_files analysis)
    | `Lint `Fmt -> `Opam_fmt (selection, Analyse.Analysis.ocamlformat_source analysis)
  in
  { label; variant; ty }

let opam_monorepo builds =
  let multi = List.compare_length_with builds 2 >= 0 in
  List.map
    (fun config ->
       let {Selection.variant; _} = Opam_monorepo.selection_of_config config in
       let label = if multi then
           Opam_monorepo.label config
         else
           Variant.to_string variant
       in
       {
         label;
         variant;
         ty = `Opam_monorepo config
       }
    )
    builds

let pp f t = Fmt.string f t.label

let label t = t.label

let pp_summary f = function
  | `Opam (`Build, _, _) -> Fmt.string f "Opam project build"
  | `Opam (`Lint `Doc, _, _) -> Fmt.string f "Opam project lint documentation"
  | `Opam (`Lint `Opam, _, _) -> Fmt.string f "Opam files lint"
  | `Opam_fmt (_, v) ->
      Fmt.pf f "ocamlformat version: %a"
        Fmt.(option ~none:(any "none") Analyse_ocamlformat.pp_source) v
  | `Opam_monorepo _ -> Fmt.string f "opam-monorepo build"
