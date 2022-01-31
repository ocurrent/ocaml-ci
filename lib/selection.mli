(** A set of packages for a single build. *)
type t = {
  variant : Variant.t;                (** The variant image to build on. *)
  packages : string list;             (** The selected packages ("name.version"). *)
  commit : string;                    (** A commit in opam-repository to use. *)
} [@@deriving yojson, ord]

val of_worker : Ocaml_ci_api.Worker.Selection.t -> t

val remove_package : t -> package:string -> t

val filter_duplicate_opam_versions : t list -> t list
