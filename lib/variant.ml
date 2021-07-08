module Ocaml_version = struct
  include Ocaml_version

  let arch_to_yojson arch = `String (string_of_arch arch)

  let arch_of_yojson j =
    match j with
    | `String a -> (
        match arch_of_string a with Ok v -> Ok v | Error _ -> Error ("unknown arch " ^ a))
    | _ -> Error "unknown json type for arch"

  let compare_arch = Stdlib.compare

  let equal_arch = ( = )

  let to_yojson t = `String (to_string t)

  let of_yojson j =
    match j with
    | `String a -> (
        match of_string a with
        | Ok v -> Ok v
        | Error _ -> Error ("unknown ocaml version " ^ a))
    | _ -> Error "unknown json for ocaml version"
end

type t = { distro : string; ocaml_version : Ocaml_version.t; arch : Ocaml_version.arch }
[@@deriving yojson, ord, eq]

let v ~arch ~distro ~ocaml_version =
  (* Just check we understand all the variants first *)
  match Ocaml_version.Configure_options.of_t ocaml_version with
  | Ok _ -> Ok { arch; distro; ocaml_version }
  | Error e -> Error e

let arch { arch; _ } = arch

let distro { distro; _ } = distro

let ocaml_version { ocaml_version; _ } = ocaml_version

let with_ocaml_version ocaml_version t = { t with ocaml_version }

let id { distro; ocaml_version; _ } =
  Fmt.strf "%s-%a" distro Ocaml_version.pp ocaml_version

let docker_tag { distro; ocaml_version; _ } =
  Fmt.strf "%s-ocaml-%s" distro
    (Ocaml_version.to_string ~sep:'-' ocaml_version
    |> String.map (function '+' -> '-' | x -> x))

let id_of_string s =
  match Astring.String.cut ~rev:true ~sep:"-" s with
  | Some (distro, ov) -> Some (distro, Ocaml_version.of_string_exn ov)
  | None -> None

let pp f t =
  Fmt.pf f "%s%s" (id t)
    (match t.arch with `X86_64 -> "" | a -> "_" ^ Ocaml_version.to_opam_arch a)

let to_string = Fmt.strf "%a" pp

let of_string s =
  let id, arch =
    match Astring.String.cut ~sep:"_" s with
    | None -> (s, `X86_64)
    | Some (s, a) -> (s, Ocaml_version.of_opam_arch a |> Option.value ~default:`X86_64)
  in
  match id_of_string id with
  | None -> raise (Failure ("internal error: unknown variant " ^ id))
  | Some (distro, ocaml_version) -> { arch; distro; ocaml_version }
