let profile =
  match Sys.getenv_opt "CI_PROFILE" with
  | Some "production" -> `Production
  | Some "dev" | None -> `Dev
  | Some x -> Fmt.failwith "Unknown $PROFILE setting %S" x

module Capnp = struct
  (* Cap'n Proto RPC is enabled by passing --capnp-public-address. These values are hard-coded
     (because they're just internal to the Docker container). *)

  let cap_secrets =
    match profile with
    | `Production -> "/capnp-secrets"
    | `Dev -> "./capnp-secrets"

  let secret_key = cap_secrets ^ "/secret-key.pem"
  let cap_file = cap_secrets ^ "/ocaml-ci-admin.cap"
  let internal_port = 9000
end

let dev_pool = Current.Pool.create ~label:"docker" 1

(** Maximum time for one Docker build. *)
let build_timeout = Duration.of_hour 1

module Builders = struct
  let v docker_context =
    let docker_context, pool =
      Some docker_context, Current.Pool.create ~label:("docker-" ^ docker_context) 20
    in
    { Ocaml_ci.Builder.docker_context; pool; build_timeout }

  let local = { Ocaml_ci.Builder.docker_context = None; pool = dev_pool; build_timeout }
end

let default_compiler = "4.10"

type platform = {
  label : string;
  builder : Ocaml_ci.Builder.t;
  pool : string;
  distro : string;
  ocaml_version : string;
  arch: Ocaml_version.arch option;
}

let platforms =
  let v ?arch label pool distro ocaml_version = { arch; label; builder = Builders.local; pool; distro; ocaml_version } in
  match profile with
  | `Production ->
    [
      (* Compiler versions:*)
      v "4.10" "linux-x86_64" "debian-10" "4.10";       (* Note: first item is also used as lint platform *)
      v "4.11" "linux-x86_64" "debian-10" "4.11";
      v "4.09" "linux-x86_64" "debian-10" "4.09";
      v "4.08" "linux-x86_64" "debian-10" "4.08";
      v "4.07" "linux-x86_64" "debian-10" "4.07";
      v "4.06" "linux-x86_64" "debian-10" "4.06";
      v "4.05" "linux-x86_64" "debian-10" "4.05";
      v "4.04" "linux-x86_64" "debian-10" "4.04";
      v "4.03" "linux-x86_64" "debian-10" "4.03";
      v "4.02" "linux-x86_64" "debian-10" "4.02";
      (* Distributions: *)
      v "alpine"   "linux-x86_64" "alpine-3.12"   default_compiler;
      v "ubuntu"   "linux-x86_64" "ubuntu-20.04"  default_compiler;
      v "opensuse" "linux-x86_64" "opensuse-15.2" default_compiler;
      v "centos"   "linux-x86_64" "centos-8"      default_compiler;
      v "fedora"   "linux-x86_64" "fedora-32"     default_compiler;
      (* oraclelinux doesn't work in opam 2 yet *)
      v ~arch:`I386 "4.10+x86_32" "linux-x86_64" "debian-10" "4.10";
      v ~arch:`Aarch32 "4.10+arm32" "linux-arm64" "debian-10" "4.10";
      v ~arch:`Aarch64 "4.10+arm64" "linux-arm64" "debian-10" "4.10";
      v ~arch:`Ppc64le "4.10+ppc64le" "linux-ppc64" "debian-10" "4.10";
    ]
  | `Dev ->
    [
      v "4.10" "linux-x86_64" "debian-10" "4.10";
      v "4.11" "linux-x86_64" "debian-10" "4.11";
      v "4.02" "linux-x86_64" "debian-10" "4.02";
      v ~arch:`I386 "4.10+32bit" "linux-x86_64" "debian-10" "4.10";
    ]
