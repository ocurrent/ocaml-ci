(** A set of packages for a single build. *)
type t = {
  variant : Variant.t;                (** The variant image to build on. *)
  packages : string list;             (** The selected packages ("name.version"). *)
  commit : string;                    (** A commit in opam-repository to use. *)
} [@@deriving yojson, ord]

let of_worker w =
  let module W = Ocaml_ci_api.Worker.Selection in
  let { W.id; packages; commits } = w in
  let variant = Variant.of_string id in
  (* The primary opam-repository commit is required to be the first in the list.
     We only pass this one through to the worker because the Docker container
     for the build has only this one cloned to local storage. *)
  let commit = List.(hd commits |> snd) in
  { variant; packages; commit }

let remove_package t ~package =
  {
    t with
    packages =
      List.filter (fun p -> not (String.equal p package)) t.packages;
  }
