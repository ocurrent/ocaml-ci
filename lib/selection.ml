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

let filter_duplicate_opam_versions ts =
  let tbl = Hashtbl.create (List.length ts) in
  let key t =
    Variant.distro t.variant,
    Variant.ocaml_version t.variant,
    Variant.arch t.variant
  in
  List.iter (fun t ->
      let opam_version = Variant.opam_version t.variant in
      let k = key t in
      let v = match Hashtbl.find_opt tbl k with
        | None -> opam_version, t
        | Some (version, v) ->
           (* keep the oldest supported opam version *)
           if Opam_version.compare opam_version version < 0 then
             (opam_version, t)
           else
             (version, v)
      in
      Hashtbl.replace tbl k v
    ) ts;
  Hashtbl.fold (fun _ (_, v) acc -> v :: acc) tbl []
