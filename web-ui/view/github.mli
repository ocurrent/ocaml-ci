module Client = Ocaml_ci_api.Client
module Common = Ocaml_ci_api.Common

val short_hash : ?first:int -> string -> string

val breadcrumbs :
  (string * string) list -> string -> [> Html_types.ol ] Tyxml_html.elt

val org_url : string -> string
val repo_url : string -> string -> string
val commit_url : org:string -> repo:string -> string -> string
val job_url : org:string -> repo:string -> hash:string -> string -> string
val github_branch_url : org:string -> repo:string -> string -> string
val github_pr_url : org:string -> repo:string -> string -> string
val format_org : string -> [> Html_types.li ] Tyxml_html.elt

val format_repo :
  org:string -> Client.Org.repo_info -> [> Html_types.li ] Tyxml_html.elt

val orgs_v : orgs:string list -> [> `Ol | `Ul ] Tyxml_html.elt list

val repos_v :
  org:string ->
  repos:Client.Org.repo_info list ->
  [> `Ol | `Ul ] Tyxml_html.elt list

val refs_v :
  org:string ->
  repo:string ->
  refs:(string * Build_status.t) Client.Ref_map.t ->
  [> Html_types.ul ] Tyxml_html.elt

val intersperse : sep:'a -> 'a list -> 'a list

val statuses :
  (Client.State.t * [< Html_types.li_content_fun ] Tyxml_html.elt list)
  Status_tree.tree
  list ->
  [> Html_types.ul ] Tyxml_html.elt

val link_github_refs :
  org:string -> repo:string -> string list -> [> `P | `PCDATA ] Tyxml_html.elt

val link_jobs :
  org:string ->
  repo:string ->
  hash:string ->
  ?selected:Client.variant ->
  Client.job_info list ->
  [> Html_types.ul ] Tyxml_html.elt

val list_orgs : orgs:string list -> string
val list_repos : org:string -> repos:Client.Org.repo_info list -> string

val list_refs :
  org:string ->
  repo:string ->
  refs:(string * Build_status.t) Client.Ref_map.t ->
  string

val cancel_success_message :
  Client.job_info list -> [> `Div | `Ul ] Tyxml_html.elt

val cancel_fail_message : int -> [> Html_types.div ] Tyxml_html.elt

val rebuild_success_message :
  Client.job_info list -> [> `Div | `Ul ] Tyxml_html.elt

val rebuild_fail_message : int -> [> Html_types.div ] Tyxml_html.elt

val return_link :
  org:string ->
  repo:string ->
  hash:string ->
  [> [> Html_types.txt ] Html_types.a ] Tyxml_html.elt

val list_steps :
  org:string ->
  repo:string ->
  refs:string list ->
  hash:string ->
  jobs:Client.job_info list ->
  ?success_msg:
    ([< Html_types.div_content_fun > `Div `Ol `P `PCDATA `Ul ] as 'a)
    Tyxml_html.elt ->
  ?fail_msg:'a Tyxml_html.elt ->
  ?return_link:'a Tyxml_html.elt ->
  ?flash_messages:(string * string) list ->
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
  ?flash_messages:(string * string) list ->
  string * int64 ->
  Dream.response Lwt.t
