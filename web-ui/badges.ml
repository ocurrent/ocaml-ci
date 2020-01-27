type schema = {
  schema_version : int; [@key "schemaVersion"]  (** always: 1 *)
  label : string;
  message : string;
  color : string option;  (** default: "lightgrey" *)
  label_color : string option; [@key "labelColor"]  (** default: "grey" *)
  is_error : bool option; [@key "isError"]  (** default: false *)
  named_logo : string option; [@key "namedLogo"]  (** default: none *)
  logo_svg : string option; [@key "logoSvg"]  (** default: none *)
  logo_color : string option; [@key "logoColor"]  (** default: none *)
  logo_width : string option; [@key "logoWidth"]  (** default: none *)
  logo_position : string option; [@key "logoPosition"]  (** default: none *)
  style : string option;  (** default: "flat" *)
  cache_seconds : int option;  (** default: 300 *)
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

let ( let* ) = Lwt.Infix.( >>= )

open Lwt.Infix
module Capability = Capnp_rpc_lwt.Capability
module Client = Ocaml_ci_api.Client
module Server = Cohttp_lwt_unix.Server

let normal_response = Lwt.map (fun x -> `Response x)

let respond_error status body =
  let headers = Cohttp.Header.init_with "Content-Type" "text/plain" in
  Server.respond_error ~status ~headers ~body () |> normal_response

let ( let*! ) x f =
  x >>= function
  | Error (`Capnp ex) ->
      respond_error `Internal_server_error
        (Fmt.to_to_string Capnp_rpc.Error.pp ex)
  | Error (`Msg msg) -> respond_error `Internal_server_error msg
  | Ok y -> f y

let handle ~backend ~path =
  let* ci = Backend.ci backend in
  match path with
  | [ org_name; repo_name; branch_name ] ->
      let ref_name = Fmt.str "refs/heads/%s" branch_name in
      Capability.with_ref (Client.CI.org ci org_name) @@ fun org ->
      Capability.with_ref (Client.Org.repo org repo_name) @@ fun repo ->
      Capability.with_ref (Client.Repo.commit_of_ref repo ref_name)
      @@ fun commit ->
      let*! status = Client.Commit.status commit in
      let body =
        status |> schema_of_status |> schema_to_yojson |> Yojson.Safe.to_string
      in
      Server.respond_string ~status:`OK ~body () |> normal_response
  | _ -> Server.respond_not_found () |> normal_response
