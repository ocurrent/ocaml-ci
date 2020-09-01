(** A set of packages for a single build. *)
type t = {
  variant : Variant.t;                (** The variant image to build on. *)
  packages : string list;             (** The selected packages ("name.version"). *)
  post_packages : string list;        (** To be installed last, dependencies with flag "post". *)
  commit : string;                    (** A commit in opam-repository to use. *)
} [@@deriving yojson, ord]

val of_worker : Ocaml_ci_api.Worker.Selection.t -> t
