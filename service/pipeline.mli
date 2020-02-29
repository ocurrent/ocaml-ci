val local_test : Current_git.Local.t -> unit -> unit Current.t
(** [local_test repo] is a pipeline that tests local repository [repo] as the CI would. *)

val v : app:Current_github.App.t -> unit -> unit Current.t
(** The main ocaml-ci pipeline. Tests everything configured for GitHub application [app]. *)
