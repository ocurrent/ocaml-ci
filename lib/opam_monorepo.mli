(** Generate obuilder specs for building opam packages with opam-monorepo. *)

type info [@@deriving yojson]

val detect : dir:Fpath.t -> info list option
(** Detect whether a project uses opam-monorepo or something else. *)

type config [@@deriving yojson, ord]

val label : config -> string
val selection_of_config : config -> Selection.t

val selection :
  info:info ->
  platforms:(Variant.t * Ocaml_ci_api.Worker.Vars.t) list ->
  solve:
    (root_pkgs:(string * string) list ->
    pinned_pkgs:(string * string) list ->
    platforms:(Variant.t * Ocaml_ci_api.Worker.Vars.t) list ->
    (Selection.t list, Rresult.R.msg) Lwt_result.t) ->
  (config, Rresult.R.msg) Lwt_result.t
(** Determine configuration for a build (which machine to run on, dune version,
    etc) *)

val spec :
  base:string ->
  repo:Repo_id.t ->
  config:config ->
  variant:Variant.t ->
  Obuilder_spec.t
(** Describe build steps for an opam-monorepo project. *)
