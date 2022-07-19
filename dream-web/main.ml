let main port backend_cap =
  Lwt_main.run
    (let open Lwt.Infix in
    Backend.Make.ci backend_cap >>= fun ci ->
    Dream.serve ~port @@ Dream.logger @@ Dream.origin_referrer_check
    @@ Dream.router
         [
           Dream.get "/css/**" @@ Dream.static "dream-web/static/css";
           Dream.get "/" (fun _ -> Dream.html @@ Controller.Index.render);
           Dream.get "/github" (fun _request -> Controller.Github.list_orgs ci);
           Dream.get "/github/:org" (fun request ->
               Controller.Github.list_repos ~org:(Dream.param request "org") ci);
           Dream.get "github/:org/:repo" (fun request ->
               Controller.Github.list_refs
                 ~org:(Dream.param request "org")
                 ~repo:(Dream.param request "repo")
                 ci);
           Dream.get "github/:org/:repo/commit/:hash" (fun request ->
               Controller.Github.list_steps
                 ~org:(Dream.param request "org")
                 ~repo:(Dream.param request "repo")
                 ~hash:(Dream.param request "hash")
                 ci);
           Dream.get "github/:org/:repo/commit/:hash/variant/:variant"
             (fun request ->
               Controller.Github.show_step
                 ~org:(Dream.param request "org")
                 ~repo:(Dream.param request "repo")
                 ~hash:(Dream.param request "hash")
                 ~variant:(Dream.param request "variant")
                 ci);
          Dream.post "github/:org/:repo/commit/:hash/variant/:variant/rebuild" (fun request ->
              Controller.Github.rebuild_step
                 ~org:(Dream.param request "org")
                 ~repo:(Dream.param request "repo")
                 ~hash:(Dream.param request "hash")
                 ~variant:(Dream.param request "variant")
                 request
                 ci);
(*
          Dream.post "github/:org/:repo/commit/:hash/cancel" (fun request ->
              Controller.Github.cancel_steps
                 ~org:(Dream.param request "org")
                 ~repo:(Dream.param request "repo")
                 ~hash:(Dream.param request "hash")
                ci);
          Dream.post "github/:org/:repo/commit/:hash/rebuild-failed" (fun request ->
              Controller.Github.rebuild_steps
                 ~rebuild_failed_only:true
                 ~org:(Dream.param request "org")
                 ~repo:(Dream.param request "repo")
                 ~hash:(Dream.param request "hash")
                ci);
          Dream.post "github/:org/:repo/commit/:hash/rebuild-all" (fun request ->
              Controller.Github.rebuild_steps
                 ~rebuild_failed_only:false
                 ~org:(Dream.param request "org")
                 ~repo:(Dream.param request "repo")
                 ~hash:(Dream.param request "hash")
                ci);
*)
         ])

open Cmdliner

let port =
  Arg.value @@ Arg.opt Arg.int 8090
  @@ Arg.info ~doc:"The port on which to listen for incoming HTTP connections."
       ~docv:"PORT" [ "port" ]

let backend_cap =
  Arg.required
  @@ Arg.opt (Arg.some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info
       ~doc:"The capability file giving access to the CI backend service."
       ~docv:"CAP" [ "backend" ]

let cmd =
  let doc = "A web front-end for OCaml-CI" in
  let info = Cmd.info "ocaml-ci-web" ~doc in
  Cmd.v info Term.(const main $ port $ backend_cap)

let () = exit @@ Cmd.eval cmd
