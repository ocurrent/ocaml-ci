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

  let amd1 = v "default"
  let amd2 = v "laodoke"
  let amd3 = v "phoebe"
  let amd4 = v "m1-a"

  let local = { Ocaml_ci.Builder.docker_context = None; pool = dev_pool; build_timeout }
end

let default_compiler = "4.10"

type platform = {
  label : string;
  builder : Ocaml_ci.Builder.t;
  distro : string;
  ocaml_version : string;
}

let platforms =
  let v label builder distro ocaml_version = { label; builder; distro; ocaml_version } in
  match profile with
  | `Production ->
    [
      (* Compiler versions:*)
      v "4.10" Builders.amd4 "debian-10" "4.10";       (* Note: first item is also used as lint platform *)
      v "4.09" Builders.amd3 "debian-10" "4.09";
      v "4.08" Builders.amd1 "debian-10" "4.08";
      v "4.07" Builders.amd2 "debian-10" "4.07";
      v "4.06" Builders.amd2 "debian-10" "4.06";
      v "4.05" Builders.amd3 "debian-10" "4.05";
      v "4.04" Builders.amd3 "debian-10" "4.04";
      v "4.03" Builders.amd2 "debian-10" "4.03";
      v "4.02" Builders.amd2 "debian-10" "4.02";
      (* Distributions: *)
      v "alpine"   Builders.amd1 "alpine-3.11"   default_compiler;
      v "ubuntu"   Builders.amd2 "ubuntu-20.04"  default_compiler;
      v "opensuse" Builders.amd2 "opensuse-15.1" default_compiler;
      v "centos"   Builders.amd3 "centos-8"      default_compiler;
      v "fedora"   Builders.amd3 "fedora-31"     default_compiler;
      (* oraclelinux doesn't work in opam 2 yet *)
    ]
  | `Dev ->
    [
      v "4.10" Builders.local "debian-10" "4.10";
    ]
