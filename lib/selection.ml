(** A set of packages for a single build. *)
type t = {
  variant : Variant.t;                (** The variant image to build on. *)
  packages : string list;             (** The selected packages ("name.version"). *)
  post_packages : string list;        (** To be installed last, dependencies with flag "post". *)
  commit : string;                    (** A commit in opam-repository to use. *)
  pin_depends : (string * string) list; (** Pinned packages. *)
} [@@deriving yojson, ord]

let make ~pin_depends w =
  let module W = Ocaml_ci_api.Worker.Selection in
  let { W.id; packages; post_packages; commit } = w in
  let variant = Variant.of_string id in
  { variant; packages; post_packages; commit; pin_depends }
