module Analysis : sig
  type t [@@deriving yojson]

  val opam_files : t -> string list
  val is_duniverse : t -> bool
  val ocamlformat_source : t -> Analyse_ocamlformat.source option
end

val examine :
  master:Current_git.Commit.t Current.t ->
  Current_git.Commit.t Current.t ->
  Analysis.t Current.t
