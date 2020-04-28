module Analysis : sig
  type t [@@deriving yojson]

  val opam_files : t -> string list
  val is_duniverse : t -> bool
  val ocamlformat_source : t -> Analyse_ocamlformat.source option

  val selections : t -> [ `Opam_build of string list | `Duniverse of string list ]
  (** Get the variants selected for the builds. *)

  val of_dir : job:Current.Job.t -> platforms:(string * Platform.Vars.t) list -> Fpath.t -> (t, [ `Msg of string ]) result Lwt.t
end

val examine : platforms:Platform.t list Current.t -> Current_git.Commit.t Current.t -> Analysis.t Current.t
(** [examine ~platforms src] analyses the source code [src]. *)
