open Lwt.Infix
open Current.Syntax

module Raw = Current_docker.Raw
module Worker = Ocaml_ci_api.Worker

let docker_tag ~distro ~ocaml_version =
  Printf.sprintf "%s-ocaml-%s" distro ocaml_version

type t = {
  label : string;
  builder : Builder.t;
  pool : string;        (* OCluster pool *)
  variant : Variant.t;
  base : Current_docker.Raw.Image.t;
  vars : Worker.Vars.t;
}

let pp f t = Fmt.string f t.label
let compare a b = compare a.label b.label

let ( >>!= ) = Lwt_result.bind

module Query = struct
  let id = "opam-vars"

  type t = No_context

  module Key = struct
    type t = {
      docker_context : string option;
      variant : Variant.t;
    } [@@deriving to_yojson]

    let digest t = Yojson.Safe.to_string (to_yojson t)
  end

  module Value = struct
    type t = {
      image : string;
      host_image: string;
    } [@@deriving to_yojson]

    let digest t = Yojson.Safe.to_string (to_yojson t)
  end

  module Outcome = struct
    type t = {
      image : string;
      vars : Worker.Vars.t;
    } [@@deriving yojson]

    let marshal t = Yojson.Safe.to_string (to_yojson t)

    let unmarshal s =
      match of_yojson (Yojson.Safe.from_string s) with
      | Ok x -> x
      | Error e -> failwith e
  end

  let opam_template arch =
    let arch = Option.value ~default:"%{arch}%" arch in
    Fmt.strf {|
    {
      "arch": "%s",
      "os": "%%{os}%%",
      "os_family": "%%{os-family}%%",
      "os_distribution": "%%{os-distribution}%%",
      "os_version": "%%{os-version}%%"
    }
  |} arch

  let get_vars ~arch docker_context image =
    Raw.Cmd.docker ~docker_context ["run"; "-i"; image; "opam"; "config"; "expand"; (opam_template arch)]

  let get_ocaml_package ~docker_context image =
    Raw.Cmd.docker ~docker_context ["run"; "-i"; image; "opam"; "list"; "-s"; "--base"; "--roots"; "--all-versions"; "ocaml-base-compiler"; "ocaml-variants"; "ocaml-system"]

  let run No_context job { Key.docker_context; variant } { Value.image; host_image } =
    Current.Job.start job ~level:Current.Level.Mostly_harmless >>= fun () ->
    let cmd = get_ocaml_package ~docker_context host_image in
    Current.Process.check_output ~cancellable:false ~job cmd >>!= fun ocaml_package ->
    let ocaml_package = String.trim ocaml_package in
    let ocaml_package_name, ocaml_version = match Astring.String.cut ~sep:"." ocaml_package with
      | Some (name, version) -> (name, version)
      | None -> Fmt.failwith "Unexpected opam package name: %s" ocaml_package
    in
    let cmd = get_vars ~arch:(Variant.(arch variant |> to_opam_arch)) docker_context host_image in
    Current.Process.check_output ~cancellable:false ~job cmd >>!= fun vars ->
    let json =
      match Yojson.Safe.from_string vars with
      | `Assoc items ->
          `Assoc (
            ("ocaml_package", `String ocaml_package_name) ::
            ("ocaml_version", `String ocaml_version) ::
            items)
      | json -> Fmt.failwith "Unexpected JSON: %a" Yojson.Safe.(pretty_print ~std:true) json
    in
    Current.Job.log job "@[<v2>Result:@,%a@]" Yojson.Safe.(pretty_print ~std:true) json;
    match Worker.Vars.of_yojson json with
    | Error msg -> Lwt_result.fail (`Msg msg)
    | Ok vars -> Lwt_result.return { Outcome.vars; image }

  let pp f (key, value) = Fmt.pf f "opam vars of %a@,(%s)" Variant.pp key.Key.variant value.Value.image

  let auto_cancel = false
  let latched = true
end

module QC = Current_cache.Generic(Query)

let query builder ~variant ~host_image image =
  Current.component "opam-vars" |>
  let> host_image = host_image
  and> image = image in
  let image = Raw.Image.hash image in
  let host_image = Raw.Image.hash host_image in
  let docker_context = builder.Builder.docker_context in
  QC.run Query.No_context { Query.Key.docker_context; variant } { Query.Value.image; host_image }

let get ~arch ~label ~builder ~pool ~distro ~ocaml_version ~host_base base =
  let variant = Variant.v ~arch (docker_tag ~distro ~ocaml_version) in
  let+ { Query.Outcome.vars; image } = query builder ~variant ~host_image:host_base base in
  let base = Raw.Image.of_hash image in
  { label; builder; pool; variant; base; vars }

let pull ~arch ~schedule ~builder ~distro ~ocaml_version =
  let archl = Variant.to_opam_arch arch |> Option.value ~default:"" in
  Current.component "pull@,%s %s %s" distro ocaml_version archl |>
  let> () = Current.return () in
  let tag = docker_tag ~distro ~ocaml_version in
  Builder.pull ~schedule ?arch builder @@ "ocurrent/opam:" ^ tag
