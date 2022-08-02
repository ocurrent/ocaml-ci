type schema = {
  schema_version : int; [@key "schemaVersion"] [@default 1]  (** always: 1 *)
  label : string;
  message : string;
  color : string option; [@default None]  (** default: "lightgrey" *)
  label_color : string option; [@key "labelColor"] [@default None]
      (** default: "grey" *)
  is_error : bool option; [@key "isError"] [@default None]
      (** default: false *)
  named_logo : string option; [@key "namedLogo"] [@default None]
  logo_svg : string option; [@key "logoSvg"] [@default None]
  logo_color : string option; [@key "logoColor"] [@default None]
  logo_width : string option; [@key "logoWidth"] [@default None]
  logo_position : string option; [@key "logoPosition"] [@default None]
  style : string option; [@default None]  (** default: "flat" *)
  cache_seconds : int option; [@default None]  (** default: 300 *)
}
[@@deriving yojson]
(** Extracted from https://shields.io/endpoint. *)

let v ~label ~message ?color ?label_color ?is_error ?named_logo ?logo_svg
    ?logo_color ?logo_width ?logo_position ?style ?cache_seconds () =
  {
    schema_version = 1;
    label;
    message;
    color;
    label_color;
    is_error;
    named_logo;
    logo_svg;
    logo_color;
    logo_width;
    logo_position;
    style;
    cache_seconds;
  }

let schema_of_status =
  let v = v ~label:"ocaml-ci" in
  function
  | `Not_started -> v ~message:"unknown" ~color:"inactive" ()
  | `Pending -> v ~message:"building" ~color:"yellow" ()
  | `Failed -> v ~message:"failing" ~color:"critical" ()
  | `Passed -> v ~message:"passing" ~color:"success" ()

open Lwt.Infix
module Capability = Capnp_rpc_lwt.Capability
module Client = Ocaml_ci_api.Client

let ( let*! ) x f =
  let respond_error msg =
    Dream.log "Failed to retrieve badge state: %s" msg;
    Dream.empty `Internal_Server_Error
  in
  x >>= function
  | Error (`Capnp ex) -> respond_error (Fmt.str "%a" Capnp_rpc.Error.pp ex)
  | Error (`Msg msg) -> respond_error msg
  | Ok y -> f y

let handle ~org ~repo ~branch ci =
  let ref_name = Fmt.str "refs/heads/%s" branch in
  Capability.with_ref (Client.CI.org ci org) @@ fun org_cap ->
  Capability.with_ref (Client.Org.repo org_cap repo) @@ fun repo_cap ->
  Capability.with_ref (Client.Repo.commit_of_ref repo_cap ref_name)
  @@ fun commit_cap ->
  let*! status = Client.Commit.status commit_cap in
  let body =
    status |> schema_of_status |> schema_to_yojson |> Yojson.Safe.to_string
  in
  Dream.json body
