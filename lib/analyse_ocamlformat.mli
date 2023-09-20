(** Detect the required version of OCamlFormat used in a source repository. *)

type source =
  | Opam of { version : string; opam_repo_commit : string option }
      (** Should install OCamlFormat from opam. *)
  | Vendored of { path : string }
      (** OCamlFormat is vendored. [path] is relative to the project's root. *)
[@@deriving yojson, eq, ord]

val pp_source : source Fmt.t
(** Pretty print [source]. *)

val get_ocamlformat_source :
  Current.Job.t ->
  opam_files:string list ->
  root:Fpath.t ->
  find_opam_repo_commit:
    (string -> (string * Selection.t, [ `Msg of string ]) Lwt_result.t) ->
  (source option * Selection.t option, [ `Msg of string ]) Lwt_result.t
(** Detect the required version of OCamlFormat or if it's vendored. Vendored
    OCamlFormat is detected by looking at file names in [opam_files]. *)
