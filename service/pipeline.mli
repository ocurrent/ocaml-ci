val local_test : solver:Ocaml_ci_api.Solver.t -> Current_git.Local.t -> unit -> unit Current.t
(** [local_test ~solver repo] is a pipeline that tests local repository [repo] as the CI would. *)

val v : app:Current_github.App.t -> solver:Ocaml_ci_api.Solver.t -> unit -> unit Current.t
(** The main ocaml-ci pipeline. Tests everything configured for GitHub application [app]. *)
