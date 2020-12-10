module Analysis : sig
  type t [@@deriving yojson]

  val opam_files : t -> string list
  val ocamlformat_source : t -> Analyse_ocamlformat.source option

  val selections : t -> [
      | `Opam_build of Selection.t list
      | `Duniverse of Variant.t list
      | `Opam_monorepo of Opam_monorepo.selection
    ]

  val of_dir :
    solver:Ocaml_ci_api.Solver.t ->
    job:Current.Job.t ->
    platforms:(Variant.t * Ocaml_ci_api.Worker.Vars.t) list ->
    opam_repository_commit:Current_git.Commit_id.t ->
    Fpath.t ->
    (t, [ `Msg of string ]) result Lwt.t
end

val examine :
  solver:Ocaml_ci_api.Solver.t ->
  platforms:Platform.t list Current.t ->
  opam_repository_commit:Current_git.Commit_id.t Current.t ->
  Current_git.Commit.t Current.t ->
  Analysis.t Current.t
(** [examine ~solver ~platforms ~opam_repository_commit src] analyses the source code [src] and selects
    package versions to test using [opam_repository_commit]. *)

val examine_vars :
  solver:Ocaml_ci_api.Solver.t ->
  vars:(Variant.t * Ocaml_ci_api.Worker.Vars.t) list Current.t ->
  opam_repository_commit:Current_git.Commit_id.t Current.t ->
  Current_git.Commit.t Current.t ->
  Analysis.t Current.t
(** [examine_vars ~solver ~vars ~opam_repository_commit src] analyses the source code [src] and selects
    package versions to test using [opam_repository_commit] -- it cuts out all of the platform information
    that is required by [examine]. *)
