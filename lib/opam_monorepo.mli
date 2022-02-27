type info

(** Detect whether a project uses opam-monorepo or something else. *)
val detect : dir:Fpath.t -> info list option

type config [@@deriving yojson, ord]

val selection_of_config : config -> Selection.t

(** Determine configuration for a build
    (which machine to run on, dune version, etc) *)
val selection :
  info:info ->
  platforms:(Variant.t * Ocaml_ci_api.Worker.Vars.t) list ->
  solve:
    (root_pkgs:(string * string) list ->
     pinned_pkgs:(string * string) list ->
     platforms:(Variant.t * Ocaml_ci_api.Worker.Vars.t) list ->
     (Selection.t list, Rresult.R.msg) Lwt_result.t) ->
  (config, Rresult.R.msg) Lwt_result.t

(** Describe build steps *)
val spec :
  base:string ->
  repo:Current_github.Repo_id.t ->
  config:config ->
  variant:Variant.t ->
  Obuilder_spec.t
