module Analysis : sig
  type ocamlformat_version = Version of string | Vendored

  type t

  val opam_files : t -> string list
  val is_duniverse : t -> bool
  val ocamlformat_version : t -> ocamlformat_version option
end

val examine : Current_git.Commit.t Current.t -> Analysis.t Current.t
(** [examine src] returns a list of "*.opam" files in [src]. *)
