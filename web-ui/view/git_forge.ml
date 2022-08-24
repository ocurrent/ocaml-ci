module type View = sig
  module Client = Ocaml_ci_api.Client
  module Run_time = Client_utilities.Run_time
  module Build_status = Build_status

  val prefix : string

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

  val list_orgs : orgs:string list -> string
  val list_repos : org:string -> repos:Client.Org.repo_info list -> string

  val list_refs :
    org:string ->
    repo:string ->
    refs:(string * Build_status.t) Client.Ref_map.t ->
    string

  val list_steps :
    org:string ->
    repo:string ->
    refs:string list ->
    hash:string ->
    jobs:Client.job_info list ->
    first_step_queued_at:float ->
    total_run_time:float ->
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
    timestamps:Run_time.timestamps option ->
    build_created_at:float option ->
    ?flash_messages:(string * string) list ->
    string * int64 ->
    Dream.response Lwt.t
end

(* Common utility functions for Git_forge views. *)
let short_hash = Astring.String.with_range ~len:6

let rec intersperse ~sep = function
  | [] -> []
  | [ x ] -> [ x ]
  | x :: xs -> x :: sep :: intersperse ~sep xs

(* Common view partials for Git_forge views. *)
open Tyxml.Html
module StatusTree = Status_tree
module Client = Ocaml_ci_api.Client

let breadcrumbs steps page_title =
  let add (prefix, results) (label, link) =
    let prefix = Fmt.str "%s/%s" prefix link in
    let link = li [ a ~a:[ a_href prefix ] [ txt label ] ] in
    (prefix, link :: results)
  in
  let _, steps = List.fold_left add ("", []) steps in
  let steps = li [ b [ txt page_title ] ] :: steps in
  ol ~a:[ a_class [ "breadcrumbs" ] ] (List.rev steps)

let statuses ss =
  let rec render_status = function
    | StatusTree.Leaf (s, elms) ->
        let status_class_name =
          match (s : Client.State.t) with
          | NotStarted -> "not-started"
          | Aborted -> "aborted"
          | Failed m when Astring.String.is_prefix ~affix:"[SKIP]" m ->
              "skipped"
          | Failed _ -> "failed"
          | Passed -> "passed"
          | Active -> "active"
          | Undefined _ -> "undefined"
        in
        li ~a:[ a_class [ status_class_name ] ] elms
    | StatusTree.Branch (b, ss) ->
        li
          [
            txt b; ul ~a:[ a_class [ "statuses" ] ] (List.map render_status ss);
          ]
  in
  ul ~a:[ a_class [ "statuses" ] ] (List.map render_status ss)
