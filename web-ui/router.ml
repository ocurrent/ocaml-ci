open Lwt.Infix

module Route (F : sig
  val prefix : string
  val request : string
  val backend : Controller.Backend.t
  val extra_routes : Dream.route list

  module Api : Api_controller.Git_forge.Api_controller
  module Controller : Controller.Git_forge.Controller
end) =
struct
  let forge =
    let url = Printf.sprintf "/%s" F.prefix in
    Dream.get url (fun request -> Dream.redirect request "/")

  let org =
    let url = Printf.sprintf "/%s/:org" F.prefix in
    Dream.get url (fun request ->
        F.Controller.list_repos ~org:(Dream.param request "org") F.backend)

  let repo =
    let url = Printf.sprintf "/%s/:org/:repo" F.prefix in
    Dream.get url (fun request ->
        F.Controller.list_refs
          ~org:(Dream.param request "org")
          ~repo:(Dream.param request "repo")
          F.backend)

  let commit =
    let url = Printf.sprintf "/%s/:org/:repo/commit/:hash" F.prefix in
    Dream.get url (fun request ->
        F.Controller.list_steps
          ~org:(Dream.param request "org")
          ~repo:(Dream.param request "repo")
          ~hash:(Dream.param request "hash")
          request F.backend)

  let variant =
    let url =
      Printf.sprintf "/%s/:org/:repo/commit/:hash/variant/:variant" F.prefix
    in
    Dream.get url (fun request ->
        F.Controller.show_step
          ~org:(Dream.param request "org")
          ~repo:(Dream.param request "repo")
          ~hash:(Dream.param request "hash")
          ~variant:(Dream.param request "variant")
          request F.backend)

  let variant_websockets =
    let url =
      Printf.sprintf "/ws/%s/:org/:repo/commit/:hash/variant/:variant" F.prefix
    in
    Dream.get url (fun request ->
        F.Controller.ws_show_step
          ~org:(Dream.param request "org")
          ~repo:(Dream.param request "repo")
          ~hash:(Dream.param request "hash")
          ~variant:(Dream.param request "variant")
          request F.backend)

  let branch_history =
    let url = Printf.sprintf "/%s/:org/:repo/history/branch/**" F.prefix in
    Dream.get url (fun request ->
        let fpath = Dream.target request |> Dream.from_path in
        let rec f = function
          | [] -> Dream.empty `Not_Found
          | "branch" :: refs ->
              let gref =
                let branch = String.concat Filename.dir_sep refs in
                `Branch branch
              in
              F.Controller.list_history
                ~org:(Dream.param request "org")
                ~repo:(Dream.param request "repo")
                ~gref F.backend
          | _ :: paths -> f paths
        in
        f fpath)

  let request_history =
    let url =
      Printf.sprintf "/%s/:org/:repo/history/%s/:number" F.prefix F.request
    in
    Dream.get url (fun request ->
        let gref =
          let id = Dream.param request "number" |> int_of_string in
          `Request id
        in
        F.Controller.list_history
          ~org:(Dream.param request "org")
          ~repo:(Dream.param request "repo")
          ~gref F.backend)

  let cancel_variant =
    let url =
      Printf.sprintf "/%s/:org/:repo/commit/:hash/variant/:variant/cancel"
        F.prefix
    in
    Dream.post url (fun request ->
        Dream.form request >>= function
        | `Ok _ ->
            F.Controller.cancel_step
              ~org:(Dream.param request "org")
              ~repo:(Dream.param request "repo")
              ~hash:(Dream.param request "hash")
              ~variant:(Dream.param request "variant")
              request F.backend
        | _ ->
            Dream.log "Form validation failed";
            Dream.empty `Bad_Request)

  let cancel_build =
    let url = Printf.sprintf "/%s/:org/:repo/commit/:hash/cancel" F.prefix in
    Dream.post url (fun request ->
        Dream.form request >>= function
        | `Ok _ ->
            F.Controller.cancel_steps
              ~org:(Dream.param request "org")
              ~repo:(Dream.param request "repo")
              ~hash:(Dream.param request "hash")
              request F.backend
        | _ ->
            Dream.log "Form validation failed";
            Dream.empty `Bad_Request)

  let rebuild_variant =
    let url =
      Printf.sprintf "/%s/:org/:repo/commit/:hash/variant/:variant/rebuild"
        F.prefix
    in
    Dream.post url (fun request ->
        Dream.form request >>= function
        | `Ok _ ->
            F.Controller.rebuild_step
              ~org:(Dream.param request "org")
              ~repo:(Dream.param request "repo")
              ~hash:(Dream.param request "hash")
              ~variant:(Dream.param request "variant")
              request F.backend
        | _ ->
            Dream.log "Form validation failed";
            Dream.empty `Bad_Request)

  let rebuild_failed_build =
    let url =
      Printf.sprintf "/%s/:org/:repo/commit/:hash/rebuild-failed" F.prefix
    in
    Dream.post url (fun request ->
        Dream.form request >>= function
        | `Ok _ ->
            F.Controller.rebuild_steps ~rebuild_failed_only:true
              ~org:(Dream.param request "org")
              ~repo:(Dream.param request "repo")
              ~hash:(Dream.param request "hash")
              request F.backend
        | _ ->
            Dream.log "Form validation failed";
            Dream.empty `Bad_Request)

  let rebuild_all_build =
    let url =
      Printf.sprintf "/%s/:org/:repo/commit/:hash/rebuild-all" F.prefix
    in
    Dream.post url (fun request ->
        Dream.form request >>= function
        | `Ok _ ->
            F.Controller.rebuild_steps ~rebuild_failed_only:false
              ~org:(Dream.param request "org")
              ~repo:(Dream.param request "repo")
              ~hash:(Dream.param request "hash")
              request F.backend
        | _ ->
            Dream.log "Form validation failed";
            Dream.empty `Bad_Request)

  let build_api =
    let url = Printf.sprintf "/api/%s/:org/:repo/commit/:hash" F.prefix in
    Dream.get url (fun request ->
        let org = Dream.param request "org" in
        let repo = Dream.param request "repo" in
        let hash = Dream.param request "hash" in
        F.Api.list_steps ~org ~repo ~hash F.backend)

  let variant_api =
    let url =
      Printf.sprintf "/api/%s/:org/:repo/commit/:hash/variant/:variant" F.prefix
    in
    Dream.get url (fun request ->
        F.Api.show_step
          ~org:(Dream.param request "org")
          ~repo:(Dream.param request "repo")
          ~hash:(Dream.param request "hash")
          ~variant:(Dream.param request "variant")
          F.backend)

  let badge =
    let url = Printf.sprintf "/badge/%s/:org/:repo/:branch" F.prefix in
    Dream.get url (fun request ->
        Controller.Badges.handle
          ~org:(Dream.param request "org")
          ~repo:(Dream.param request "repo")
          ~branch:(Dream.param request "branch")
          F.backend)

  let routes () =
    [
      forge;
      org;
      repo;
      commit;
      variant;
      branch_history;
      request_history;
      cancel_variant;
      cancel_build;
      rebuild_failed_build;
      rebuild_all_build;
      rebuild_variant;
      build_api;
      variant_api;
      variant_websockets;
      badge;
    ]
    @ F.extra_routes
end

(* ocaml-crunch is used to generate the module Static.
   See also https://github.com/aantron/dream/tree/master/example/w-one-binary *)
let loader ?(caching = false) root path _request =
  Dream.log "In loader. root: %s path: %s" root path;
  match Static.read (Filename.concat root path) with
  | None -> Dream.empty `Not_Found
  | Some asset ->
      let headers =
        if caching then
          (* Kept in cache for a month. The ocaml-ci changes are regular so we don't keep it too long. . *)
          [ ("Cache-Control", "max-age=2419200") ]
        else []
      in
      Dream.respond ~headers asset

let static =
  [
    Dream.get "/css/ansi.css" (fun _ ->
        Dream.respond ~headers:[ ("content-type", "text/css") ] View.Common.css);
    Dream.get "/favicon.ico" @@ Dream.static ~loader "/";
    Dream.get "/css/**" @@ Dream.static ~loader "/css";
    Dream.get "/images/**" @@ Dream.static ~loader "/images";
    Dream.get "/js/**" @@ Dream.static ~loader "/js";
    Dream.get "/fonts/**"
    @@ Dream.static ~loader:(loader ~caching:true) "/fonts";
    Dream.get "/profile-pictures/**" @@ Dream.static "profile-pictures";
  ]

let documentation =
  [
    Dream.get "/getting-started" (fun _ ->
        Dream.html @@ Controller.Documentation.getting_started);
    Dream.get "/documentation" (fun _ ->
        Dream.html @@ Controller.Documentation.user_guide);
  ]

let root ~gitlab ~github =
  [
    Dream.get "/" (fun _ ->
        match (github, gitlab) with
        | None, None ->
            Dream.log "No backend available";
            Dream.empty `Internal_Server_Error
        | Some github, None ->
            let orgs = [ ("github", "GitHub", github) ] in
            Controller.Index.list_orgs ~orgs
        | None, Some gitlab ->
            Controller.Index.list_orgs ~orgs:[ ("gitlab", "GitLab", gitlab) ]
        | Some github, Some gitlab ->
            let orgs =
              [ ("github", "GitHub", github); ("gitlab", "GitLab", gitlab) ]
            in
            Controller.Index.list_orgs ~orgs);
  ]

let build_gitlab_route gitlab =
  let module Gitlab = Route (struct
    let prefix = "gitlab"
    let request = "merge-request"
    let backend = gitlab
    let extra_routes = []

    module Api = Api_controller.Gitlab
    module Controller = Controller.Gitlab
  end) in
  Gitlab.routes ()

let build_github_route github =
  let module Github = Route (struct
    let prefix = "github"
    let request = "pull"
    let backend = github

    (* Extra routes are here to keep legacy compatibility. *)
    let extra_routes =
      [
        Dream.get "/badge/:org/:repo/:branch" (fun request ->
            let target =
              Printf.sprintf "/badge/github/%s/%s/%s"
                (Dream.param request "org")
                (Dream.param request "repo")
                (Dream.param request "branch")
            in
            Dream.redirect request target);
        Dream.get "/github/:org/:repo/commit/:hash/-/**" (fun request ->
            let target =
              List.hd (Astring.String.cuts ~sep:"/-/" (Dream.target request))
            in
            Dream.redirect request target);
        Dream.get "/github/:org/:repo/commit/:hash/variant/:variant/-/**"
          (fun request ->
            let target =
              List.hd (Astring.String.cuts ~sep:"/-/" (Dream.target request))
            in
            Dream.redirect request target);
        (* Dream.get "/ws/github/:org/:repo/commit/:hash/variant/:variant"
           (fun request ->
             Ws_controller.Github.show_step
               ~org:(Dream.param request "org")
               ~repo:(Dream.param request "repo")
               ~hash:(Dream.param request "hash")
               ~variant:(Dream.param request "variant")
               request backend); *)
      ]

    module Api = Api_controller.Github
    module Controller = Controller.Github
  end) in
  Github.routes ()

let create ~github ~gitlab =
  let gitlab_route =
    match gitlab with None -> [] | Some gitlab -> build_gitlab_route gitlab
  in
  let github_route =
    match github with None -> [] | Some github -> build_github_route github
  in
  Dream.router
    (root ~gitlab ~github @ static @ documentation @ gitlab_route @ github_route)
