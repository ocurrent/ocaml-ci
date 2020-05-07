open Lwt.Infix
open Current.Syntax

module Raw = Current_docker.Raw
module Worker = Ocaml_ci_api.Worker

let docker_tag ~distro ~ocaml_version =
  Printf.sprintf "%s-ocaml-%s" distro ocaml_version

type t = {
  label : string;
  builder : Builder.t;
  variant : string;
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
      variant : string;
    } [@@deriving to_yojson]

    let digest t = Yojson.Safe.to_string (to_yojson t)
  end

  module Value = struct
    type t = {
      image : string;
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

  let opam_template = {|
    {
      "arch": "%{arch}%",
      "os": "%{os}%",
      "os_family": "%{os-family}%",
      "os_distribution": "%{os-distribution}%",
      "os_version": "%{os-version}%"
    }
  |}

  let get_vars ~docker_context image =
    Raw.Cmd.docker ~docker_context ["run"; "-i"; image; "opam"; "config"; "expand"; opam_template]

  let get_ocaml_version ~docker_context image =
    Raw.Cmd.docker ~docker_context ["run"; "-i"; image; "opam"; "exec"; "--"; "ocaml"; "-vnum"]

  let run No_context job { Key.docker_context; variant = _ } { Value.image } =
    Current.Job.start job ~level:Current.Level.Mostly_harmless >>= fun () ->
    let cmd = get_ocaml_version ~docker_context image in
    Current.Process.check_output ~cancellable:false ~job cmd >>!= fun vnum ->
    let ocaml_version = String.trim vnum in
    let cmd = get_vars ~docker_context image in
    Current.Process.check_output ~cancellable:false ~job cmd >>!= fun vars ->
    let json =
      match Yojson.Safe.from_string vars with
      | `Assoc items -> `Assoc (("ocaml_version", `String ocaml_version) :: items)
      | json -> Fmt.failwith "Unexpected JSON: %a" Yojson.Safe.(pretty_print ~std:true) json
    in
    Current.Job.log job "@[<v2>Result:@,%a@]" Yojson.Safe.(pretty_print ~std:true) json;
    match Worker.Vars.of_yojson json with
    | Error msg -> Lwt_result.fail (`Msg msg)
    | Ok vars -> Lwt_result.return { Outcome.vars; image }

  let pp f (key, value) = Fmt.pf f "opam vars of %s@,(%s)" key.Key.variant value.Value.image

  let auto_cancel = false
  let latched = true
end

module QC = Current_cache.Generic(Query)

let query builder ~variant image =
  Current.component "opam-vars" |>
  let> image = image in
  let image = Raw.Image.hash image in
  let docker_context = builder.Builder.docker_context in
  QC.run Query.No_context { Query.Key.docker_context; variant } { Query.Value.image }

let get ~label ~builder ~distro ~ocaml_version base =
  let variant = docker_tag ~distro ~ocaml_version in
  let+ { Query.Outcome.vars; image } = query builder base ~variant in
  let base = Raw.Image.of_hash image in
  { label; builder; variant; base; vars }

let pull ~schedule ~builder ~distro ~ocaml_version =
  Current.component "pull@,%s %s" distro ocaml_version |>
  let> () = Current.return () in
  let tag = docker_tag ~distro ~ocaml_version in
  Builder.pull ~schedule builder @@ "ocurrent/opam:" ^ tag
