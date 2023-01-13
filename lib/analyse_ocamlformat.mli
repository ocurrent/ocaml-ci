(** Detect the required version of ocamlformat used in a source repository. *)

type source =
  | Opam of { version : string; opam_repo_commit : string }
      (** Should install OCamlformat from Opam. *)
  | Vendored of { path : string }
      (** OCamlformat is vendored. [path] is relative to the project's root. *)
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
(** Detect the required version of OCamlformat or if it's vendored. Vendored
    OCamlformat is detected by looking at file names in [opam_files]. *)
