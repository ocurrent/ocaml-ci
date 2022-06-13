module Analysis : sig
  type t [@@deriving yojson]

  val opam_files : t -> string list
  val ocamlformat_selection : t -> Selection.t option
  val ocamlformat_source : t -> Analyse_ocamlformat.source option

  val selections : t -> [
      | `Opam_build of Selection.t list
      | `Opam_monorepo of Opam_monorepo.config list
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
