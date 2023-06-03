(** Selected set of packages for a single build. *)

type t = {
  variant : Variant.t;  (** The variant image to build on. *)
  packages : string list;  (** The selected packages ("name.version"). *)
  compatible_root_pkgs : string list;  (** Local root packages to include. *)
  commit : string;  (** A commit in opam-repository to use. *)
}
[@@deriving yojson, ord]
(** A set of packages for a single build. *)

val of_worker : Ocaml_ci_api.Worker.Selection.t -> t

val remove_package : t -> package:string -> t
(** [remove_package t] by package name from the selection. *)

val filter_duplicate_opam_versions : t list -> t list
(** [filter_duplicate_opam_versions] from the list. *)
