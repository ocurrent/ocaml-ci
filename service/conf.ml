(** Pipeline configuration. *)

let ci_profile =
  match Sys.getenv_opt "CI_PROFILE" with
  | Some "production" -> `Production
  | Some "dev" | None -> `Dev
  | Some x -> Fmt.failwith "Unknown $CI_PROFILE setting %S." x

let cmdliner_envs =
  let values = [ "production"; "dev" ] in
  let doc =
    Printf.sprintf "CI profile settings, must be %s."
      (Cmdliner.Arg.doc_alts values)
  in
  [ Cmdliner.Cmd.Env.info "CI_PROFILE" ~doc ]

(* GitHub defines a stale branch as more than 3 months old.
   Don't bother testing these. *)
let max_staleness = Duration.of_day 93

module Capnp = struct
  (* Cap'n Proto RPC is enabled by passing --capnp-public-address. These values are hard-coded
     (because they're just internal to the Docker container). *)

  let cap_secrets =
    match ci_profile with
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
      ( Some docker_context,
        Current.Pool.create ~label:("docker-" ^ docker_context) 20 )
    in
    { Ocaml_ci.Builder.docker_context; pool; build_timeout }

  let local =
    { Ocaml_ci.Builder.docker_context = None; pool = dev_pool; build_timeout }
end

module OV = Ocaml_version
module DD = Dockerfile_opam.Distro

let default_compilers =
  OV.(List.map with_just_major_and_minor Releases.[ v4_14; latest ])

let trunk_compiler = OV.(Sources.trunk |> without_patch)

type platform = {
  label : string;
  builder : Ocaml_ci.Builder.t;
  pool : string;
  distro : string;
  ocaml_version : OV.t;
  arch : OV.arch;
  opam_version : Ocaml_ci.Opam_version.t;
}

(* TODO Hardcoding the versions for now, this should expand to OV.Releases.recent.
   Currently we only have base images for these 2 compiler variants. See ocurrent/macos-infra playbook.yml.
*)
let macos_distros : platform list =
  [
    {
      label = "macos-homebrew";
      builder = Builders.local;
      pool = "macos-x86_64";
      distro = "macos-homebrew";
      ocaml_version = OV.Releases.v4_14;
      arch = `X86_64;
      opam_version = `V2_1;
    };
    {
      label = "macos-homebrew";
      builder = Builders.local;
      pool = "macos-x86_64";
      distro = "macos-homebrew";
      ocaml_version = OV.Releases.v5_0;
      arch = `X86_64;
      opam_version = `V2_1;
    };
    (* Apple Silicon *)
    {
      label = "macos-homebrew";
      builder = Builders.local;
      pool = "macos-arm64";
      distro = "macos-homebrew";
      ocaml_version = OV.Releases.v4_14;
      arch = `Aarch64;
      opam_version = `V2_1;
    };
    {
      label = "macos-homebrew";
      builder = Builders.local;
      pool = "macos-arm64";
      distro = "macos-homebrew";
      ocaml_version = OV.Releases.v5_0;
      arch = `Aarch64;
      opam_version = `V2_1;
    };
  ]

let pool_of_arch = function
  | `X86_64 | `I386 -> "linux-x86_64"
  | `Aarch32 | `Aarch64 -> "linux-arm64"
  | `S390x -> "linux-s390x"
  | `Ppc64le -> "linux-ppc64"
  | `Riscv64 -> "linux-riscv64"

let platforms ~ci_profile ~include_macos opam_version =
  let v ?(arch = `X86_64) label distro ocaml_version =
    {
      arch;
      label;
      builder = Builders.local;
      pool = pool_of_arch arch;
      distro;
      ocaml_version;
      opam_version;
    }
  in
  let master_distro = DD.resolve_alias DD.master_distro in
  let make_distro distro =
    let distro = DD.resolve_alias distro in
    let label = DD.latest_tag_of_distro (distro :> DD.t) in
    let tag = DD.tag_of_distro (distro :> DD.t) in
    let f ov =
      if distro = master_distro then
        v label tag (OV.with_variant ov (Some "flambda"))
        :: List.map
             (fun arch -> v ~arch label tag ov)
             (DD.distro_arches ov (distro :> DD.t))
      else [ v label tag ov ]
    in
    List.fold_left (fun l ov -> f ov @ l) [] default_compilers
  in
  let make_release ?arch ov =
    let distro = DD.tag_of_distro (master_distro :> DD.t) in
    let ov = OV.with_just_major_and_minor ov in
    v ?arch (OV.to_string ov) distro ov
  in
  match ci_profile with
  | `Production ->
      let distros =
        DD.active_tier1_distros `X86_64 @ DD.active_tier2_distros `X86_64
        |> List.map make_distro
        |> List.flatten
      in
      let distros =
        if include_macos then macos_distros @ distros else distros
      in
      (* The first one in this list is used for lint actions *)
      let ovs = List.rev OV.Releases.recent @ OV.Releases.unreleased_betas in
      List.map make_release ovs @ distros
  | `Dev when Sys.win32 ->
      (* Assume we're building using native Windows images. *)
      let distro =
        DD.tag_of_distro (`Windows (`Mingw, DD.win10_latest_image) :> DD.t)
      in
      let ov = OV.with_just_major_and_minor OV.Releases.latest in
      [ v (OV.to_string ov) distro ov ]
  | `Dev ->
      let[@warning "-8"] (latest :: previous :: _) =
        List.rev OV.Releases.recent
      in
      let ovs = [ latest; previous ] in
      let macos_distros = if include_macos then macos_distros else [] in
      List.map make_release ovs @ macos_distros

let fetch_platforms ~include_macos () =
  let open Ocaml_ci in
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 30) () in
  let v { label; builder; pool; distro; ocaml_version; arch; opam_version } =
    match distro with
    | "macos-homebrew" ->
        (* TODO No docker images for macos yet, lets pretend. *)
        let docker_image_name =
          Fmt.str "%s-ocaml-%d.%d" distro (OV.major ocaml_version)
            (OV.minor ocaml_version)
        in
        let label =
          Fmt.str "pull %s %s" docker_image_name (OV.string_of_arch arch)
        in
        let base = Current.return ~label (`MacOS docker_image_name) in
        Platform.get_macos ~arch ~label ~builder ~pool ~distro ~ocaml_version
          ~opam_version base
    | _ ->
        let base =
          Platform.pull ~arch ~schedule ~builder ~distro ~ocaml_version
            ~opam_version
        in
        let host_base =
          match arch with
          | `X86_64 -> base
          | _ ->
              Platform.pull ~arch:`X86_64 ~schedule ~builder ~distro
                ~ocaml_version ~opam_version
        in
        Platform.get ~arch ~label ~builder ~pool ~distro ~ocaml_version
          ~host_base ~opam_version base
  in
  let v2_1 = platforms ~ci_profile `V2_1 ~include_macos in
  Current.list_seq (List.map v v2_1)
