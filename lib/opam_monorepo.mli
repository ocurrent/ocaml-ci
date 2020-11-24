type info

(** Detect whether a project uses opam-monorepo or something else. *)
val detect : dir:Fpath.t -> info option

(** Find a machine that can build for a given compiler. *)
val find_compiler :
  platforms:(Variant.t * Ocaml_ci_api.Worker.Vars.t) list ->
  version:string ->
  Variant.t option

type config [@@deriving yojson, ord]

type selection = Variant.t * config [@@deriving yojson]

(** Determine configuration for a build
    (which machine to run on, dune version, etc) *)
val selections :
  platforms:(Variant.t * Ocaml_ci_api.Worker.Vars.t) list ->
  info:info ->
  selection option

(** Describe build steps *)
val spec :
  base:string ->
  repo:Current_github.Repo_id.t ->
  spec:config ->
  variant:Variant.t ->
  Obuilder_spec.stage
