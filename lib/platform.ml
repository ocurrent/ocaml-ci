open Lwt.Infix
open Current.Syntax
module Raw = Current_docker.Raw
module Worker = Ocaml_ci_api.Worker

type base = [ `Docker of Current_docker.Raw.Image.t | `MacOS of string ]
(* TODO Use docker images for bundling macos binaries. *)

let to_yojson = function
  | `Docker image ->
      `List [ `String "docker"; `String (Raw.Image.digest image) ]
  | `MacOS s -> `List [ `String "macos"; `String s ]

let to_string = function
  | `Docker image -> Raw.Image.hash image
  | `MacOS s -> "macos-" ^ s

let base_pp f = function
  | `Docker image -> Fmt.pf f "%a" Raw.Image.pp image
  | `MacOS s -> Fmt.pf f "%s" s

type t = {
  label : string;
  builder : Builder.t;
  pool : string; (* OCluster pool *)
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

  type t = No_context

  module Key = struct
    type t = { docker_context : string option; variant : Variant.t }
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
  let prepare_image ~job ~docker_context ~tag variant image =
    let opam =
      "opam-" ^ Opam_version.to_string (Variant.opam_version variant)
    in
    let prefix =
      match Variant.os variant with `macOS -> "~/local" | `linux -> "/usr"
    in
    let ln =
      match Variant.os variant with `macOS -> "ln" | `linux -> "sudo ln"
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

  let get_vars ~arch ~docker_context image =
    Raw.Cmd.docker ~docker_context
      [ "run"; "-i"; image; "opam"; "config"; "expand"; opam_template arch ]

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

  let run No_context job { Key.docker_context; variant }
      { Value.image; host_image } =
    Current.Job.start job ~level:Current.Level.Mostly_harmless >>= fun () ->
    let prep_image = Fmt.str "ocurrent/ocaml-ci:%s" (Variant.docker_tag variant) in
    prepare_image ~job ~docker_context ~tag:prep_image variant host_image
    >>!= fun host_image ->
    let cmd = get_ocaml_package ~docker_context host_image in
    Current.Process.check_output ~cancellable:false ~job cmd
    >>!= fun ocaml_package ->
    let ocaml_package = String.trim ocaml_package in
    let ocaml_package_name, ocaml_version =
      match Astring.String.cut ~sep:"." ocaml_package with
      | Some (name, version) -> (name, version)
      | None -> Fmt.failwith "Unexpected opam package name: %s" ocaml_package
    in
    let arch =
      Variant.arch variant |> fun v ->
      if Ocaml_version.arch_is_32bit v then Some (Ocaml_version.to_opam_arch v)
      else None
    in
    let cmd = get_vars ~arch ~docker_context host_image in
    Current.Process.check_output ~cancellable:false ~job cmd >>!= fun vars ->
    let json =
      match Yojson.Safe.from_string vars with
      | `Assoc items ->
          `Assoc
            (("ocaml_package", `String ocaml_package_name)
            :: ("ocaml_version", `String ocaml_version)
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

let query builder ~variant ~host_image image =
  Current.component "opam-vars"
  |> let> host_image = host_image and> image = image in
     let image = Raw.Image.hash image in
     let host_image = Raw.Image.hash host_image in
     let docker_context = builder.Builder.docker_context in
     QC.run Query.No_context
       { Query.Key.docker_context; variant }
       { Query.Value.image; host_image }

let get_docker builder variant host_base base arch opam_version label pool =
  let+ { Query.Outcome.vars; image } =
    query builder ~variant ~host_image:host_base base
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

let get ~arch ~label ~builder ~pool ~distro ~ocaml_version ~host_base
    ~opam_version base =
  match Variant.v ~arch ~distro ~ocaml_version ~opam_version with
  | Error (`Msg m) -> Current.fail m
  | Ok variant ->
      get_docker builder variant host_base base arch opam_version label pool

let get_macos ~arch ~label ~builder ~pool ~distro ~ocaml_version ~opam_version
    base =
  (* Hardcoding opam-vars for macos 12.6 Monterey. *)
  match Variant.v ~arch ~distro ~ocaml_version ~opam_version with
  | Error (`Msg m) -> Current.fail m
  | Ok variant ->
      Current.component "opam-vars"
      |> let** (`MacOS s) = base in
         let vars =
           {
             Worker.Vars.arch = Ocaml_version.to_opam_arch arch;
             os = "macos";
             os_family = "macos";
             os_distribution = "macos";
             os_version = "12.6";
             ocaml_package = "ocaml-base-compiler";
             ocaml_version = Fmt.str "%a" Ocaml_version.pp ocaml_version;
             opam_version = Opam_version.to_string_with_patch opam_version;
           }
         in
         Current.return { label; builder; pool; variant; base = `MacOS s; vars }

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
