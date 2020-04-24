open Lwt.Infix
open Current.Syntax

module Raw = Current_docker.Raw

module Vars = struct
  type t = {
    arch : string;
    os : string;
    os_family : string;
    os_distribution : string;
    os_version : string;
  } [@@deriving yojson]

  let template = {|
    {
      "arch": "%{arch}%",
      "os": "%{os}%",
      "os_family": "%{os-family}%",
      "os_distribution": "%{os-distribution}%",
      "os_version": "%{os-version}%"
    }
  |}

  let marshal t = Yojson.Safe.to_string (to_yojson t)

  let unmarshal s =
    match of_yojson (Yojson.Safe.from_string s) with
    | Ok x -> x
    | Error e -> failwith e
end

type t = {
  label : string;
  builder : Builder.t;
  variant : string;
  base : Current_docker.Raw.Image.t;
  vars : Vars.t;
}

let pp f t = Fmt.string f t.label
let compare a b = compare a.label b.label

module Query = struct
  let id = "opam-vars"

  type t = No_context

  module Key = struct
    type t = {
      docker_context : string option;
      image : string;
    } [@@deriving to_yojson]

    let digest t = Yojson.Safe.to_string (to_yojson t)
  end

  module Value = Vars

  let get_vars ~docker_context image =
    Raw.Cmd.docker ~docker_context ["run"; "-i"; image; "opam"; "config"; "expand"; Vars.template]

  let build No_context job { Key.docker_context; image } =
    Current.Job.start job ~level:Current.Level.Mostly_harmless >>= fun () ->
    let cmd = get_vars ~docker_context image in
    Current.Process.check_output ~cancellable:false ~job cmd >|= function
    | Error _ as e -> e
    | Ok vars ->
      match Vars.of_yojson (Yojson.Safe.from_string vars) with
      | Error msg ->
        Current.Job.log job "Output was: %S" vars;
        failwith msg
      | Ok vars ->
        Current.Job.log job "@[<v2>Result:@,%a@]" Yojson.Safe.(pretty_print ~std:true) (Value.to_yojson vars);
        Ok vars

  let pp f key = Fmt.pf f "opam vars of %s" key.Key.image

  let auto_cancel = false
end

module QC = Current_cache.Make(Query)

let query builder image =
  Current.component "opam-vars" |>
  let> image = image in
  let image = Raw.Image.hash image in
  let docker_context = builder.Builder.docker_context in
  QC.get Query.No_context { Query.Key.docker_context; image }

let get ~label ~builder ~variant base =
  let+ vars = query builder base
  and+ base = base in
  { label; builder; variant; base; vars }
