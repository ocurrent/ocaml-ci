(** A set of packages for a single build. *)
type t = {
  variant : Variant.t;                (** The variant image to build on. *)
  packages : string list;             (** The selected packages ("name.version"). *)
  commit : string;                    (** A commit in opam-repository to use. *)
} [@@deriving yojson, ord]

let of_worker w =
  let module W = Ocaml_ci_api.Worker.Selection in
  let { W.id; packages; commit } = w in
  let variant = Variant.of_string id in
  { variant; packages; commit }

let remove_package t ~package =
  {
    t with
    packages =
      List.filter (fun p -> not (String.equal p package)) t.packages;
  }
