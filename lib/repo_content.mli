(** Extract from a source repository the informations that needed to analyse the
    opam packages. *)
module Content : sig
  type t [@@deriving yojson]

  val opam_files : t -> string list
  val root_pkgs : t -> (string * string) list
  val pinned_pkgs : t -> (string * string) list
  val ocamlformat_version : t -> string option
  val dir_type : t -> [ `Opam_monorepo of Opam_monorepo.info list | `Ocaml_repo ]
  val marshal : t -> string
  val unmarshal : string -> t

  val of_dir :
    job:Current.Job.t -> Fpath.t -> (t, [ `Msg of string ]) result Lwt.t
end

val extract : Current_git.Commit.t Current.t -> Content.t Current.t
(** [extract src] extract the informations of the source code [src], usefull for
    analysis *)
