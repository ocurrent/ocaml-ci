module Analysis : sig
  type ocamlformat_version = Version of string | Vendored
  [@@deriving yojson, eq]

  type t

  val opam_files : t -> string list
  val is_duniverse : t -> bool
  val ocamlformat_version : t -> ocamlformat_version option

  val of_dir : job:Current.Job.t -> Fpath.t -> (t, [ `Msg of string ]) result Lwt.t
end

val examine : Current_git.Commit.t Current.t -> Analysis.t Current.t
(** [examine src] returns a list of "*.opam" files in [src]. *)
