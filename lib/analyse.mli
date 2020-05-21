module Analysis : sig
  type t [@@deriving yojson]

  val opam_files : t -> string list
  val is_duniverse : t -> bool
  val ocamlformat_source : t -> Analyse_ocamlformat.source option

  val selections : t -> [
      | `Opam_build of Ocaml_ci_api.Worker.Selection.t list
      | `Duniverse of string list               (* Variants to build on *)
    ]

  val of_dir :
    solver:Ocaml_ci_api.Solver.t ->
    job:Current.Job.t ->
    platforms:(string * Ocaml_ci_api.Worker.Vars.t) list ->
    opam_repository_commit:Current_git.Commit_id.t ->
    Fpath.t ->
    (t, [ `Msg of string ]) result Lwt.t
end

val examine :
  solver:Ocaml_ci_api.Solver.t ->
  platforms:Platform.t list Current.t ->
  opam_repository:Current_git.Commit.t Current.t ->
  Current_git.Commit.t Current.t ->
  Analysis.t Current.t
(** [examine ~solver ~platforms ~opam_repository src] analyses the source code [src] and selects
    package versions to test using [opam_repository]. *)
