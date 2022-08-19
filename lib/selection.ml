type t = {
  variant : Variant.t;  (** The variant image to build on. *)
  packages : string list;  (** The selected packages ("name.version"). *)
  only_packages : string list; [@default []]
      (** Local root packages to include (empty to include all). *)
  commit : string;  (** A commit in opam-repository to use. *)
}
[@@deriving yojson, ord]
(** A set of packages for a single build. *)

let of_worker ~root_pkgs w =
  let module W = Ocaml_ci_api.Worker.Selection in
  let { W.id; compat_pkgs; packages; commit } = w in
  let variant = Variant.of_string id in
  let only_packages = if root_pkgs = compat_pkgs then [] else compat_pkgs in
  { variant; only_packages; packages; commit }

let remove_package t ~package =
  {
    t with
    packages = List.filter (fun p -> not (String.equal p package)) t.packages;
  }

let filter_duplicate_opam_versions ts =
  let tbl = Hashtbl.create (List.length ts) in
  let key t =
    ( Variant.distro t.variant,
      Variant.ocaml_version t.variant,
      Variant.arch t.variant )
  in
  List.iter
    (fun t ->
      let opam_version = Variant.opam_version t.variant in
      let k = key t in
      let v =
        match Hashtbl.find_opt tbl k with
        | None -> (opam_version, t)
        | Some (version, v) ->
            (* keep the oldest supported opam version *)
            if Opam_version.compare opam_version version < 0 then
              (opam_version, t)
            else (version, v)
      in
      Hashtbl.replace tbl k v)
    ts;
  Hashtbl.fold (fun _ (_, v) acc -> v :: acc) tbl []
