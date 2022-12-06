module Client = Ocaml_ci_api.Client
module Run_time = Ocaml_ci_client_lib.Run_time
module Build_status = Build_status

module type Forge_prefix = sig
  val prefix : string
end

module type Forge = sig
  include Forge_prefix

  val request_abbrev : string
  val org_url : org:string -> string
  val repo_url : org:string -> repo:string -> string
  val commit_url : org:string -> repo:string -> hash:string -> string
  val branch_url : org:string -> repo:string -> string -> string
  val request_url : org:string -> repo:string -> string -> string

  val parse_ref :
    string -> [ `Branch of string | `Request of int | `Unknown of string ]
end

module type View = sig
  val prefix : string

  val cancel_success_message :
    Client.job_info list -> [> `Div | `Ul ] Tyxml_html.elt

  val cancel_fail_message : int -> [> Html_types.div ] Tyxml_html.elt

  val rebuild_success_message :
    Client.job_info list -> [> `Div | `Ul ] Tyxml_html.elt

  val rebuild_fail_message : int -> [> Html_types.div ] Tyxml_html.elt
  val cancel_fail_message_v1 : int -> ([> `Fail ] * string) list

  val success_message_v1 :
    [< `Cancel | `Rebuild ] ->
    Client.job_info list ->
    ([> `Success ] * string) list

  val rebuild_fail_message_v1 : int -> ([> `Fail ] * string) list
  val list_repos : org:string -> repos:Client.Org.repo_info list -> string

  val list_refs :
    org:string ->
    repo:string ->
    default_ref:string ->
    refs:Client.Repo.ref_info Client.Ref_map.t ->
    string

  val list_history :
    org:string ->
    repo:string ->
    ref:string ->
    history:Client.Repo.ref_info list ->
    string

  val list_steps :
    org:string ->
    repo:string ->
    message:string ->
    refs:string list ->
    hash:string ->
    jobs:Client.job_info list ->
    first_step_queued_at:float option ->
    total_run_time:float ->
    build_run_time:float ->
    ?flash_messages:(string * string) list ->
    ?build_status:Client.State.t ->
    csrf_token:string ->
    unit ->
    string

  val show_step :
    org:string ->
    repo:string ->
    refs:string list ->
    hash:string ->
    jobs:Client.job_info list ->
    variant:Client.variant ->
    job:Current_rpc.Job.t ->
    status:Current_rpc.Job.status ->
    csrf_token:string ->
    timestamps:Run_time.timestamps option ->
    build_created_at:float option ->
    ?flash_messages:(string * string) list ->
    string * int64 ->
    Dream.response Lwt.t
end

module type S = sig
  module Client = Client

  module type Forge_prefix = Forge_prefix
  module type Forge = Forge
  module type View = View

  module Make (_ : Forge) : View
end
