type schema = {
  schema_version : int;
  label : string;
  message : string;
  color : string option;
  label_color : string option;
  is_error : bool option;
  named_logo : string option;
  logo_svg : string option;
  logo_color : string option;
  logo_width : string option;
  logo_position : string option;
  style : string option;
  cache_seconds : int option;
}

val schema_to_yojson : schema -> Yojson.Safe.t

val schema_of_yojson :
  Yojson.Safe.t -> schema Ppx_deriving_yojson_runtime.error_or

val v :
  label:string ->
  message:string ->
  ?color:string ->
  ?label_color:string ->
  ?is_error:bool ->
  ?named_logo:string ->
  ?logo_svg:string ->
  ?logo_color:string ->
  ?logo_width:string ->
  ?logo_position:string ->
  ?style:string ->
  ?cache_seconds:int ->
  unit ->
  schema

val schema_of_status : [ `Failed | `Not_started | `Passed | `Pending ] -> schema

module Capability = Capnp_rpc_lwt.Capability
module Client = Ocaml_ci_api.Client

val ( let*! ) :
  ( 'a,
    [< `Capnp of [< `Cancelled | `Exception of Capnp_rpc.Exception.t ]
    | `Msg of string ] )
  result
  Lwt.t ->
  ('a -> Dream.response Lwt.t) ->
  Dream.response Lwt.t

val handle :
  org:string ->
  repo:string ->
  branch:string ->
  Client.CI.t ->
  Dream.response Lwt.t
