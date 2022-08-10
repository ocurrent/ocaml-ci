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
} [@@deriving yojson]

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

module Client = Ocaml_ci_api.Client

val handle :
  org:string ->
  repo:string ->
  branch:string ->
  Client.CI.t ->
  Dream.response Lwt.t
