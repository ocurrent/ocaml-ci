open Lwt.Infix
open Current.Syntax
module Raw = Current_docker.Raw
module Worker = Ocaml_ci_api.Worker

type base =
  [ `Docker of Current_docker.Raw.Image.t
  | `MacOS of string
  | `FreeBSD of string ]
(* TODO Use docker images for bundling macos binaries. *)

let to_yojson = function
  | `Docker image ->
      `List [ `String "docker"; `String (Raw.Image.digest image) ]
  | `MacOS s -> `List [ `String "macos"; `String s ]
  | `FreeBSD s -> `List [ `String "freebsd"; `String s ]

let to_string = function
  | `Docker image -> Raw.Image.hash image
  | `MacOS s -> "macos-" ^ s
  | `FreeBSD s -> "freebsd-" ^ s

let base_pp f = function
  | `Docker image -> Fmt.pf f "%a" Raw.Image.pp image
  | `MacOS s -> Fmt.pf f "%s" s
  | `FreeBSD s -> Fmt.pf f "%s" s

(* OCluster pool name *)
module Pool_name = struct
  type t =
    [ `Linux_x86_64
    | `Linux_ARM64
    | `Linux_ppc64
    | `Linux_s390x
    | `Linux_riscv64
    | `Macos_x86_64
    | `Macos_ARM64
    | `FreeBSD_x86_64 ]

  let to_string = function
    | `Linux_x86_64 -> "linux-x86_64"
    | `Linux_ARM64 -> "linux-arm64"
    | `Linux_ppc64 -> "linux-ppc64"
    | `Linux_s390x -> "linux-s390x"
    | `Linux_riscv64 -> "linux-riscv64"
    | `Macos_x86_64 -> "macos-x86_64"
    | `Macos_ARM64 -> "macos-arm64"
    | `FreeBSD_x86_64 -> "freebsd-x86_64"

  let of_string = function
    | "linux-x86_64" -> Ok `Linux_x86_64
    | "linux-arm64" -> Ok `Linux_ARM64
    | "linux-ppc64" -> Ok `Linux_ppc64
    | "linux-s390x" -> Ok `Linux_s390x
    | "linux-riscv64" -> Ok `Linux_riscv64
    | "macos-x86_64" -> Ok `Macos_x86_64
    | "macos-arm64" -> Ok `Macos_ARM64
    | "freebsd-x86_64 " -> Ok `FreeBSD_x86_64
    | s -> Error (`Msg (s ^ ": invalid pool name"))
end

type t = {
  label : string;
  builder : Builder.t;
  pool : Pool_name.t; (* OCluster pool *)
  variant : Variant.t;
  base : base;
  vars : Worker.Vars.t;
}

let pp f t = Fmt.string f t.label
let compare a b = compare a.label b.label
let ( >>!= ) = Lwt_result.bind

let compiler_matches_major_and_minor vars ~version =
  let vars_version =
    Ocaml_version.with_just_major_and_minor
      (Ocaml_version.of_string_exn vars.Worker.Vars.ocaml_version)
  in
  Ocaml_version.equal vars_version
    (Ocaml_version.with_just_major_and_minor version)

let set_compiler_version vars ~version =
  let ocaml_version = Ocaml_version.to_string version in
  { vars with Worker.Vars.ocaml_version }

module Query = struct
  let id = "opam-vars"

  type t = {
    conn : Current_ocluster.Connection.t;
    pool : unit Current.Pool.t
  }

  module Key = struct
    type t = {
      docker_context : string option;
      variant : Variant.t;
      lower_bound : bool;
    }
    [@@deriving to_yojson]

    let digest t = Yojson.Safe.to_string (to_yojson t)
  end

  module Value = struct
    type t = { image : string; host_image : string } [@@deriving to_yojson]

    let digest t = Yojson.Safe.to_string (to_yojson t)
  end

  module Outcome = struct
    type t = { image : string; vars : Worker.Vars.t } [@@deriving yojson]

    let marshal t = Yojson.Safe.to_string (to_yojson t)

    let unmarshal s =
      match of_yojson (Yojson.Safe.from_string s) with
      | Ok x -> x
      | Error e -> failwith e
  end

  (* This is needed iff the opam used isn't the image default opam. *)
  (*
  let prepare_image ~job ~docker_context ~tag variant image =
    let opam =
      "opam-" ^ Opam_version.to_string (Variant.opam_version variant)
    in
    let prefix =
      match Variant.os variant with
      | `macOS -> "~/local"
      | `linux -> "/usr"
      | `freeBSD -> "/usr/local"
    in
    let ln =
      match Variant.os variant with
      | `macOS -> "ln"
      | `linux | `freeBSD -> "sudo ln"
    in
    (* XXX: don't overwrite default config? *)
    let opamrc = "" in
    let spec =
      let open Obuilder_spec in
      stage ~from:image
        [
          run "%s -f %s/bin/%s %s/bin/opam" ln prefix opam prefix;
          run "opam init --reinit%s -ni" opamrc;
        ]
      |> Docker.dockerfile_of_spec ~buildkit:true ~os:`Unix
    in
    let cmd =
      Raw.Cmd.docker ~docker_context [ "build"; "--pull"; "-t"; tag; "-" ]
    in
    Current.Process.exec ~stdin:spec ~cancellable:false ~job cmd >>!= fun () ->
    Lwt_result.ok (Lwt.return tag)
     *)

  (* This is needed iff the opam used isn't the image default opam. *)
  let prepare_image2 ~variant =
    let opam =
      "opam-" ^ Opam_version.to_string (Variant.opam_version variant)
    in
    let prefix =
      match Variant.os variant with
      | `macOS -> "~/local"
      | `linux -> "/usr"
      | `freeBSD -> "/usr/local"
    in
    let ln =
      match Variant.os variant with
      | `macOS -> "ln"
      | `linux | `freeBSD -> "sudo ln"
    in
    (* XXX: don't overwrite default config? *)
    let opamrc = "" in
    let open Obuilder_spec in
        [
          run "%s -f %s/bin/%s %s/bin/opam" ln prefix opam prefix;
          run "opam init --reinit%s -ni" opamrc;
        ]

  let opam_template arch =
    let arch = Option.value ~default:"%{arch}%" arch in
    Fmt.str
      {|
    {
      "arch": "%s",
      "os": "%%{os}%%",
      "os_family": "%%{os-family}%%",
      "os_distribution": "%%{os-distribution}%%",
      "os_version": "%%{os-version}%%",
      "opam_version": "%%{opam-version}%%"
    }
  |}
      arch

  (*
  let get_vars ~arch ~docker_context image =
    Raw.Cmd.docker ~docker_context
      [ "run"; "-i"; image; "opam"; "config"; "expand"; opam_template arch ]
     *)

(*
  let get_ocaml_package ~docker_context image =
    Raw.Cmd.docker ~docker_context
      [
        "run";
        "-i";
        image;
        "opam";
        "list";
        "-s";
        "--base";
        "--roots";
        "--all-versions";
        "ocaml-base-compiler";
        "ocaml-variants";
        "ocaml-system";
      ]
   *)

  let tail ~buffer ~job build_job =
    let rec aux start =
      Cluster_api.Job.log build_job start >>= function
      | Error (`Capnp e) -> Lwt.return @@ Fmt.error_msg "%a" Capnp_rpc.Error.pp e
      | Ok ("", _) -> Lwt_result.return ()
      | Ok (data, next) ->
        Buffer.add_string buffer data;
        Current.Job.write job data;
        aux next
    in aux 0L

  let run_job ~buffer ~job build_job =
    let on_cancel _ =
      Cluster_api.Job.cancel build_job >|= function
      | Ok () -> ()
      | Error (`Capnp e) -> Current.Job.log job "Cancel failed: %a" Capnp_rpc.Error.pp e
    in
    Current.Job.with_handler job ~on_cancel @@ fun () ->
    let result = Cluster_api.Job.result build_job in
    tail ~buffer ~job build_job >>!= fun () ->
    result >>= function
    | Error (`Capnp e) -> Lwt_result.fail (`Msg (Fmt.to_to_string Capnp_rpc.Error.pp e))
    | Ok _ as x -> Lwt.return x

  let parse_output job build_job =
    let buffer = Buffer.create 1024 in
    Capnp_rpc_lwt.Capability.with_ref build_job (run_job ~buffer ~job) >>!= fun (_ : string) ->
      match Astring.String.cuts ~sep:"\n@@@OUTPUT\n" (Buffer.contents buffer) with
      | [_; output; _; output2; _] ->
        Current.Job.log job "@[<v2>Result:@,%s,%s@]" output output2;
        Lwt_result.return (Some (output, output2))
      | [_; rest ] when Astring.String.is_prefix ~affix:"@@@OUTPUT\n" rest -> Lwt_result.return None
      | _ -> Lwt_result.fail (`Msg "Missing output from command\n")

  let set_personality ~variant =
    if Variant.arch variant |> Ocaml_version.arch_is_32bit then
      [Obuilder_spec.shell ["/usr/bin/linux32"; "/bin/sh"; "-c"]]
    else
      []

  let run { conn; pool } job { Key.docker_context; variant; lower_bound }
      { Value.image; host_image } =
    let open Obuilder_spec in
    let _ = pool in
    let _ = docker_context in
    let arch =
      Variant.arch variant |> fun v ->
      if Ocaml_version.arch_is_32bit v then Some (Ocaml_version.to_opam_arch v)
      else None
    in
    let spec = Obuilder_spec.stage ~from:host_image (
      user_unix ~uid:1000 ~gid:1000 ::
      set_personality ~variant @
      prepare_image2 ~variant @ [
        run "echo '@@@OUTPUT' && \
             opam list -s --color=never --base --roots --all-versions ocaml-base-compiler ocaml-variants ocaml-system && \
             echo '@@@OUTPUT'";
        run "echo '@@@OUTPUT' && \
             opam config expand '%s' && \
             echo '@@@OUTPUT'" (opam_template arch)
      ]) in
    let spec_str = Fmt.to_to_string Obuilder_spec.pp (spec) in
    let action = Cluster_api.Submission.obuilder_build spec_str in
    let foo_pool = Current_ocluster.Connection.pool ~job ~pool:"linux-x86_64" ~cache_hint:"foo" ~action conn in
    Current.Job.start_with ~pool:foo_pool job ~level:Current.Level.Mostly_harmless >>=
    parse_output job >>!= fun s ->
    let (ocaml_package, vars) = match s with
      | Some (a, b) -> a, b 
      | None -> assert false in
    (*
    Current.Job.start job ~pool ~level:Current.Level.Mostly_harmless
    >>= fun () ->
    let prep_image =
      Fmt.str "ocurrent/ocaml-ci:%s" (Variant.docker_tag variant)
    in
    prepare_image ~job ~docker_context ~tag:prep_image variant host_image
    >>!= fun host_image ->
    let cmd = get_ocaml_package ~docker_context host_image in
    Current.Process.check_output ~cancellable:false ~job cmd
    >>!= fun ocaml_package ->
       *)
    let ocaml_package = String.trim ocaml_package in
    let ocaml_package_name, ocaml_version =
      match Astring.String.cut ~sep:"." ocaml_package with
      | Some (name, version) -> (name, version)
      | None -> Fmt.failwith "Unexpected opam package name: %s" ocaml_package
    in
    (*
    let cmd = get_vars ~arch ~docker_context host_image in
    Current.Process.check_output ~cancellable:false ~job cmd >>!= fun vars ->
       *)
    let json =
      match Yojson.Safe.from_string vars with
      | `Assoc items ->
          `Assoc
            (("ocaml_package", `String ocaml_package_name)
            :: ("ocaml_version", `String ocaml_version)
            :: ("lower_bound", `Bool lower_bound)
            :: items)
      | json ->
          Fmt.failwith "Unexpected JSON: %a"
            Yojson.Safe.(pretty_print ~std:true)
            json
    in
    Current.Job.log job "@[<v2>Result:@,%a@]"
      Yojson.Safe.(pretty_print ~std:true)
      json;
    match Worker.Vars.of_yojson json with
    | Error msg -> Lwt_result.fail (`Msg msg)
    | Ok vars -> Lwt_result.return { Outcome.vars; image }

  let pp f (key, value) =
    Fmt.pf f "opam vars of %a@,(%s)" Variant.pp key.Key.variant
      value.Value.image

  let auto_cancel = false
  let latched = true
end

module QC = Current_cache.Generic (Query)

let query conn builder ~variant ~lower_bound ~host_image image =
  let label = if lower_bound then "opam-vars (lower-bound)" else "opam-vars" in
  Current.component "%s" label
  |> let> host_image and> image in
     let image = Raw.Image.hash image in
     let host_image = Raw.Image.hash host_image in
     let docker_context = builder.Builder.docker_context in
     let pool = builder.Builder.pool in
     QC.run { conn; pool }
       { Query.Key.docker_context; variant; lower_bound }
       { Query.Value.image; host_image }

let get_docker conn builder variant ~lower_bound host_base base arch opam_version
    label pool =
  let+ { Query.Outcome.vars; image } =
    query conn builder ~variant ~lower_bound ~host_image:host_base base
  in
  (* It would be better to run the opam query on the platform itself, but for
     now we run everything on x86_64 and then assume that the other
     architectures are the same except for the arch flag. *)
  let vars =
    {
      vars with
      arch = Ocaml_version.to_opam_arch arch;
      opam_version = Opam_version.to_string_with_patch opam_version;
    }
  in
  let base = Raw.Image.of_hash image in
  { label; builder; pool; variant; base = `Docker base; vars }

let get ~arch ~label ~conn ~builder ~pool ~distro ~ocaml_version ~host_base
    ~opam_version ~lower_bound base =
  match Variant.v ~arch ~distro ~ocaml_version ~opam_version with
  | Error (`Msg m) -> Current.fail m
  | Ok variant ->
      let upper_bound =
        get_docker conn builder variant ~lower_bound:false host_base base arch
          opam_version label pool
      in
      if lower_bound then
        let lower_bound =
          get_docker conn builder variant ~lower_bound:true host_base base arch
            opam_version label pool
        in
        Current.list_seq [ upper_bound; lower_bound ]
      else Current.list_seq [ upper_bound ]

let latest_ocaml_version ~ocaml_version =
  match
    (Ocaml_version.major ocaml_version, Ocaml_version.minor ocaml_version)
  with
  | 4, 14 -> Ocaml_version.to_string Ocaml_version.Releases.v4_14
  | 5, 1 -> Ocaml_version.to_string Ocaml_version.Releases.v5_1
  | _ -> Ocaml_version.to_string Ocaml_version.Releases.v5_2

let get_macos ~arch ~label ~builder ~pool ~distro ~ocaml_version ~opam_version
    ~lower_bound base =
  (* Hardcoding opam-vars for macos 14.1 Sonoma *)
  match Variant.v ~arch ~distro ~ocaml_version ~opam_version with
  | Error (`Msg m) -> Current.fail m
  | Ok variant ->
      Current.component "opam-vars"
      |> let** (`MacOS s) = base in
         let vars =
           {
             Worker.Vars.arch = Ocaml_version.to_opam_arch arch;
             os = "macos";
             os_family = "homebrew";
             os_distribution = "homebrew";
             os_version = "14.1";
             ocaml_package = "ocaml-base-compiler";
             ocaml_version = latest_ocaml_version ~ocaml_version;
             opam_version = Opam_version.to_string_with_patch opam_version;
             lower_bound;
           }
         in
         Current.return
           [ { label; builder; pool; variant; base = `MacOS s; vars } ]

let get_freebsd ~arch ~label ~builder ~pool ~distro ~ocaml_version ~opam_version
    ~lower_bound base =
  (* Hardcoding opam-vars for FreeBSD 14.0. *)
  match Variant.v ~arch ~distro ~ocaml_version ~opam_version with
  | Error (`Msg m) -> Current.fail m
  | Ok variant ->
      Current.component "opam-vars"
      |> let** (`FreeBSD s) = base in
         let vars =
           {
             Worker.Vars.arch = Ocaml_version.to_opam_arch arch;
             os = "freebsd";
             os_family = "bsd";
             os_distribution = "freebsd";
             os_version = "1400097";
             ocaml_package = "ocaml-base-compiler";
             ocaml_version = latest_ocaml_version ~ocaml_version;
             opam_version = Opam_version.to_string_with_patch opam_version;
             lower_bound;
           }
         in
         Current.return
           [ { label; builder; pool; variant; base = `FreeBSD s; vars } ]

let pull ~arch ~schedule ~builder ~distro ~ocaml_version ~opam_version =
  match Variant.v ~arch ~distro ~ocaml_version ~opam_version with
  | Error (`Msg m) -> Current.fail m
  | Ok variant ->
      let archl = Ocaml_version.to_opam_arch arch in
      let opam_version = Opam_version.to_string opam_version in
      Current.component "pull@,%s %a %s opam-%s" distro Ocaml_version.pp
        ocaml_version archl opam_version
      |> let> () = Current.return () in
         let tag = Variant.docker_tag variant in
         Builder.pull ~schedule ~arch builder @@ "ocaml/opam:" ^ tag
