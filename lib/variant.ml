module Ocaml_version = struct
  include Ocaml_version
  let arch_to_yojson arch = `String (string_of_arch arch)
  let arch_of_yojson j =
    match j with
    |`String a -> begin
       match arch_of_string a with
       | Ok v -> Ok v
       | Error _ -> Error ("unknown arch " ^ a)
     end
    | _ -> Error "unknown json type for arch"

  let compare_arch = Stdlib.compare
  let equal_arch = (=)
  let to_yojson t = `String (to_string t)
  let of_yojson j =
    match j with
    |`String a -> begin
      match of_string a with
      | Ok v -> Ok v
      | Error _ -> Error ("unknown ocaml version " ^ a)
    end
    | _ -> Error "unknown json for ocaml version"
end

type t = {
  distro: string;
  ocaml_version: Ocaml_version.t;
  arch: Ocaml_version.arch;
  opam_version: Opam_version.t;
} [@@deriving yojson, ord, eq]

let v ~arch ~distro ~ocaml_version ~opam_version =
  (* Just check we understand all the variants first *)
  match Ocaml_version.Configure_options.of_t ocaml_version with
  | Ok _ -> Ok { arch; distro; ocaml_version; opam_version }
  | Error e -> Error e

let arch { arch; _ } = arch
let distro { distro; _ } = distro
let ocaml_version { ocaml_version; _ } = ocaml_version
let with_ocaml_version ocaml_version t = { t with ocaml_version }
let opam_version t = t.opam_version

let id { distro; ocaml_version; _ } =
  Fmt.str "%s-%a" distro Ocaml_version.pp ocaml_version

let docker_tag { distro; ocaml_version; _ } =
  Fmt.str "%s-ocaml-%s" distro (Ocaml_version.to_string ~sep:'-' ocaml_version
    |> String.map (function | '+' -> '-' | x -> x))

let id_of_string s =
  match Astring.String.cut ~rev:true ~sep:"-" s with
  | Some (distro, ov) -> Some (distro, (Ocaml_version.of_string_exn ov))
  | None -> None

let pp f t =
  Fmt.pf f "%s%s%s" (id t)
    (match t.arch with
     | `X86_64 -> ""
     | a -> "_" ^ (Ocaml_version.to_opam_arch a))
    (match t.opam_version with
     | `V2_0 -> ""
     | v -> "_opam-" ^ (Opam_version.to_string v))

let to_string =
  Fmt.str "%a" pp

let of_string s =
  let id, arch, opam_version =
   match Astring.String.cut ~sep:"_" s with
   | None -> s, `X86_64, Opam_version.default
   | Some (s, a) ->
      let arch a = Ocaml_version.of_opam_arch a |> Option.value ~default:`X86_64 in
      match Astring.String.cut ~rev:true ~sep:"_opam-" a with
      | None -> s, arch a, Opam_version.default
      | Some (a, v) ->
         match Opam_version.of_string v with
         | Ok v -> s, arch a, v
         | Error _ -> s, arch a, Opam_version.default
  in
  match id_of_string id with
  | None -> raise (Failure ("internal error: unknown variant " ^ id))
  | Some (distro, ocaml_version) -> { arch; distro; ocaml_version; opam_version }
