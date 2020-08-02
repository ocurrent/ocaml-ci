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

module OV = Ocaml_version

let default_compiler = OV.(Releases.latest |> without_patch)
let trunk_compiler = OV.(Sources.trunk |> without_patch)

type platform = {
  label : string;
  builder : Ocaml_ci.Builder.t;
  pool : string;
  distro : string;
  ocaml_version : OV.t;
  arch: OV.arch;
}

let platforms =
  let make ?(arch=`X86_64) label distro ocaml_version =
    let pool = match arch with
      | `X86_64 | `I386 -> "linux-x86_64"
      | `Aarch32 | `Aarch64 -> "linux-arm64"
      | `Ppc64le -> "linux-ppc64" in
    { arch; label; builder = Builders.local; pool; distro; ocaml_version }
  in
  let make_distro_variants arch label distro =
    (* expand out a distro to the latest default version and trunk *)
    let trunk =
      List.map (make ~arch label distro)
        (List.map OV.without_patch (OV.trunk_variants arch)) in
    let default = make ~arch label distro default_compiler in
    default :: trunk in
  let make_distro arch label distro =
    [ make ~arch label distro default_compiler;
      make ~arch label distro trunk_compiler] in
  let distros l = List.map (fun (arch, distro, variants) ->
    let label = Dockerfile_distro.latest_tag_of_distro distro in
    let distro = Dockerfile_distro.tag_of_distro distro in
    if variants then
      make_distro_variants arch label distro
    else
      make_distro arch label distro) l |> List.flatten
  in
  let v ?arch ov =
    let distro = Dockerfile_distro.tag_of_distro (`Debian `V10) in
    make ?arch (OV.to_string ov) distro ov in
  let releases ?arch l = List.map (v ?arch) l in
  match profile with
  | `Production ->
      (* Distributions: *)
      distros [
        `X86_64,  `Debian `V10, true;
        `I386,    `Debian `V10, true;
        `Aarch64, `Debian `V10, true;
        `Aarch32, `Debian `V10, true;
        `Ppc64le, `Debian `V10, true;
        `X86_64,  `Alpine `V3_12, false;
        `X86_64,  `Ubuntu `V20_04, false;
        `X86_64,  `Ubuntu `V18_04, false;
        `X86_64,  `OpenSUSE `V15_2, false;
        `X86_64,  `CentOS `V8, false;
        `X86_64,  `Fedora `V32, false ] @
      (* Compiler versions:*)
      releases (List.map OV.without_patch (OV.Releases.recent |> List.rev))
  | `Dev ->
      releases (List.map OV.of_string_exn ["4.10"; "4.11"; "4.12"; "4.03"])
    @ releases ~arch:`I386 [OV.of_string_exn "4.10"]
