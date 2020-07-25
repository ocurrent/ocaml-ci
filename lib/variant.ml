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

  let to_opam_arch = function
  | `I386 -> "x86_32"
  | `X86_64 -> "x86_64"
  | `Ppc64le -> "ppc64"
  | `Aarch32 -> "arm32"
  | `Aarch64 -> "arm64"

  let to_docker_arch = function
   | `I386 -> "386"
   | `X86_64 -> "amd64"
   | `Ppc64le -> "ppc64le"
   | `Aarch32 -> "arm"
   | `Aarch64 -> "arm64"

  let of_opam_arch = function
  | "x86_32" -> Some `I386
  | "x86_64" -> Some `X86_64
  | "ppc64" -> Some `Ppc64le
  | "arm32" -> Some `Aarch32
  | "arm64" -> Some `Aarch64
  | _ -> None

end

let to_opam_arch a = Option.map Ocaml_version.to_opam_arch a
let to_docker_arch a = Option.map Ocaml_version.to_docker_arch a

type t = string * Ocaml_version.arch option [@@deriving yojson, ord, eq]
let v ~arch n = n, arch
let arch = snd
let id = fst

let pp f (id,arch) =
  Fmt.pf f "%s%s" id
    (match arch with
     | None -> ""
     | Some a -> "_" ^ (Ocaml_version.to_opam_arch a))

let to_string =
  Fmt.strf "%a" pp

let of_string s =
   match Astring.String.cut ~sep:"_" s with
   | None -> s, None
   | Some (s, a) -> s, (Ocaml_version.of_opam_arch a)


