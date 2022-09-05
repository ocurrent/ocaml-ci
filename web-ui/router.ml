open Lwt.Infix

(* ocaml-crunch is used to generate the module Static.
   See also https://github.com/aantron/dream/tree/master/example/w-one-binary *)
let loader root path _request =
  match Static.read (Filename.concat root path) with
  | None -> Dream.empty `Not_Found
  | Some asset -> Dream.respond asset

let create ~github ~gitlab =
  Dream.router
    [
      Dream.get "/css/ansi.css" (fun _ ->
          Dream.respond ~headers:[ ("content-type", "text/css") ] Ansi.css);
      Dream.get "/css/**" @@ Dream.static ~loader "/css";
      Dream.get "/images/**" @@ Dream.static ~loader "/images";
      Dream.get "/badge/:org/:repo/:branch" (fun request ->
          Controller.Badges.handle
            ~org:(Dream.param request "org")
            ~repo:(Dream.param request "repo")
            ~branch:(Dream.param request "branch")
            github);
      Dream.get "/badge/gitlab/:org/:repo/:branch" (fun request ->
          Controller.Badges.handle
            ~org:(Dream.param request "org")
            ~repo:(Dream.param request "repo")
            ~branch:(Dream.param request "branch")
            gitlab);
      Dream.get "/" (fun _ -> Dream.html @@ Controller.Index.render);
      Dream.get "/gitlab" (fun _ -> Controller.Gitlab.list_orgs gitlab);
      Dream.get "/gitlab/:org" (fun request ->
          Controller.Gitlab.list_repos ~org:(Dream.param request "org") gitlab);
      Dream.get "/gitlab/:org/:repo" (fun request ->
          Controller.Gitlab.list_refs
            ~org:(Dream.param request "org")
            ~repo:(Dream.param request "repo")
            gitlab);
      Dream.get "/gitlab/:org/:repo/commit/:hash" (fun request ->
          Controller.Gitlab.list_steps
            ~org:(Dream.param request "org")
            ~repo:(Dream.param request "repo")
            ~hash:(Dream.param request "hash")
            request gitlab);
      Dream.get "/gitlab/:org/:repo/commit/:hash/variant/:variant"
        (fun request ->
          Controller.Gitlab.show_step
            ~org:(Dream.param request "org")
            ~repo:(Dream.param request "repo")
            ~hash:(Dream.param request "hash")
            ~variant:(Dream.param request "variant")
            request gitlab);
      Dream.post "/gitlab/:org/:repo/commit/:hash/variant/:variant/rebuild"
        (fun request ->
          Dream.form request >>= function
          | `Ok _ ->
              Controller.Gitlab.rebuild_step
                ~org:(Dream.param request "org")
                ~repo:(Dream.param request "repo")
                ~hash:(Dream.param request "hash")
                ~variant:(Dream.param request "variant")
                request gitlab
          | _ ->
              Dream.log "Form validation failed";
              Dream.empty `Bad_Request);
      Dream.post "/gitlab/:org/:repo/commit/:hash/cancel" (fun request ->
          Dream.form request >>= function
          | `Ok _ ->
              Controller.Gitlab.cancel_steps
                ~org:(Dream.param request "org")
                ~repo:(Dream.param request "repo")
                ~hash:(Dream.param request "hash")
                request gitlab
          | _ ->
              Dream.log "Form validation failed";
              Dream.empty `Bad_Request);
      Dream.post "/gitlab/:org/:repo/commit/:hash/rebuild-failed"
        (fun request ->
          Dream.form request >>= function
          | `Ok _ ->
              Controller.Gitlab.rebuild_steps ~rebuild_failed_only:true
                ~org:(Dream.param request "org")
                ~repo:(Dream.param request "repo")
                ~hash:(Dream.param request "hash")
                request gitlab
          | _ ->
              Dream.log "Form validation failed";
              Dream.empty `Bad_Request);
      Dream.post "/gitlab/:org/:repo/commit/:hash/rebuild-all" (fun request ->
          Dream.form request >>= function
          | `Ok _ ->
              Controller.Gitlab.rebuild_steps ~rebuild_failed_only:false
                ~org:(Dream.param request "org")
                ~repo:(Dream.param request "repo")
                ~hash:(Dream.param request "hash")
                request gitlab
          | _ ->
              Dream.log "Form validation failed";
              Dream.empty `Bad_Request);
      Dream.get "/github" (fun _ -> Controller.Github.list_orgs github);
      Dream.get "/github/:org" (fun request ->
          Controller.Github.list_repos ~org:(Dream.param request "org") github);
      Dream.get "/github/:org/:repo" (fun request ->
          Controller.Github.list_refs
            ~org:(Dream.param request "org")
            ~repo:(Dream.param request "repo")
            github);
      Dream.get "/github/:org/:repo/commit/:hash" (fun request ->
          Controller.Github.list_steps
            ~org:(Dream.param request "org")
            ~repo:(Dream.param request "repo")
            ~hash:(Dream.param request "hash")
            request github);
      Dream.get "/github/:org/:repo/commit/:hash/variant/:variant"
        (fun request ->
          Controller.Github.show_step
            ~org:(Dream.param request "org")
            ~repo:(Dream.param request "repo")
            ~hash:(Dream.param request "hash")
            ~variant:(Dream.param request "variant")
            request github);
      Dream.post "/github/:org/:repo/commit/:hash/variant/:variant/rebuild"
        (fun request ->
          Dream.form request >>= function
          | `Ok _ ->
              Controller.Github.rebuild_step
                ~org:(Dream.param request "org")
                ~repo:(Dream.param request "repo")
                ~hash:(Dream.param request "hash")
                ~variant:(Dream.param request "variant")
                request github
          | _ ->
              Dream.log "Form validation failed";
              Dream.empty `Bad_Request);
      Dream.post "/github/:org/:repo/commit/:hash/cancel" (fun request ->
          Dream.form request >>= function
          | `Ok _ ->
              Controller.Github.cancel_steps
                ~org:(Dream.param request "org")
                ~repo:(Dream.param request "repo")
                ~hash:(Dream.param request "hash")
                request github
          | _ ->
              Dream.log "Form validation failed";
              Dream.empty `Bad_Request);
      Dream.post "/github/:org/:repo/commit/:hash/rebuild-failed"
        (fun request ->
          Dream.form request >>= function
          | `Ok _ ->
              Controller.Github.rebuild_steps ~rebuild_failed_only:true
                ~org:(Dream.param request "org")
                ~repo:(Dream.param request "repo")
                ~hash:(Dream.param request "hash")
                request github
          | _ ->
              Dream.log "Form validation failed";
              Dream.empty `Bad_Request);
      Dream.post "/github/:org/:repo/commit/:hash/rebuild-all" (fun request ->
          Dream.form request >>= function
          | `Ok _ ->
              Controller.Github.rebuild_steps ~rebuild_failed_only:false
                ~org:(Dream.param request "org")
                ~repo:(Dream.param request "repo")
                ~hash:(Dream.param request "hash")
                request github
          | _ ->
              Dream.log "Form validation failed";
              Dream.empty `Bad_Request);
    ]
