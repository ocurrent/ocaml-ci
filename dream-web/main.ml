open Lwt.Infix
module Router = struct

  (* ocaml-crunch is used to generate the module Static.
   See also https://github.com/aantron/dream/tree/master/example/w-one-binary *)
  let loader root path _request =
    match Static.read (Filename.concat root path) with
    | None -> Dream.empty `Not_Found
    | Some asset -> Dream.respond asset

  let t ci = Dream.router
         [
           Dream.get "/css/ansi.css" (fun _ -> Dream.respond ~headers:[("content-type", "text/css")] Ansi.css);
           Dream.get "/css/**" @@ (Dream.static ~loader "/css");
           Dream.get "/badge/:org/:repo/:branch" @@ (fun request ->
               Controller.Badges.handle
                 ~org:(Dream.param request "org")
                 ~repo:(Dream.param request "repo")
                 ~branch:(Dream.param request "branch")
                 ci);
           Dream.get "/" (fun _ -> Dream.html @@ Controller.Index.render);
           Dream.get "/github" (fun _ -> Controller.Github.list_orgs ci);
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
                 request ci);
           Dream.get "github/:org/:repo/commit/:hash/variant/:variant"
             (fun request ->
               Controller.Github.show_step
                 ~org:(Dream.param request "org")
                 ~repo:(Dream.param request "repo")
                 ~hash:(Dream.param request "hash")
                 ~variant:(Dream.param request "variant")
                 request ci);
           Dream.post "github/:org/:repo/commit/:hash/variant/:variant/rebuild"
             (fun request ->
               Dream.form request >>= function
               | `Ok _ ->
                   Controller.Github.rebuild_step
                     ~org:(Dream.param request "org")
                     ~repo:(Dream.param request "repo")
                     ~hash:(Dream.param request "hash")
                     ~variant:(Dream.param request "variant")
                     request ci
               | _ ->
                   Dream.log "Form validation failed";
                   Dream.empty `Bad_Request);
           Dream.post "github/:org/:repo/commit/:hash/cancel" (fun request ->
               Dream.form request >>= function
               | `Ok _ ->
                   Controller.Github.cancel_steps
                     ~org:(Dream.param request "org")
                     ~repo:(Dream.param request "repo")
                     ~hash:(Dream.param request "hash")
                     request ci
               | _ ->
                   Dream.log "Form validation failed";
                   Dream.empty `Bad_Request);
           Dream.post "github/:org/:repo/commit/:hash/rebuild-failed"
             (fun request ->
               Dream.form request >>= function
               | `Ok _ ->
                   Controller.Github.rebuild_steps ~rebuild_failed_only:true
                     ~org:(Dream.param request "org")
                     ~repo:(Dream.param request "repo")
                     ~hash:(Dream.param request "hash")
                     request ci
               | _ ->
                   Dream.log "Form validation failed";
                   Dream.empty `Bad_Request);
           Dream.post "github/:org/:repo/commit/:hash/rebuild-all"
             (fun request ->
               Dream.form request >>= function
               | `Ok _ ->
                   Controller.Github.rebuild_steps ~rebuild_failed_only:false
                     ~org:(Dream.param request "org")
                     ~repo:(Dream.param request "repo")
                     ~hash:(Dream.param request "hash")
                     request ci
               | _ ->
                   Dream.log "Form validation failed";
                   Dream.empty `Bad_Request);
         ]
end

let setup_logs default_level =
  Prometheus_unix.Logging.init ?default_level ()
  let () = Dream.initialize_log ()

let main interface port backend_cap prometheus_config log_level =
  Lwt_main.run begin
    let () = setup_logs log_level in
    Backend.Make.ci backend_cap >>= fun ci ->
    let web = Dream.serve ~interface ~port ~error_handler:(Dream.error_template View.Client_error.ocaml_ci_error_template)
    @@ Dream.logger
    @@ Dream.memory_sessions
    @@ Dream.flash
    @@ Router.t ci
    in
    Lwt.choose (web :: Prometheus_unix.serve prometheus_config)
  end

open Cmdliner

let interface =
  Arg.value
  @@ Arg.opt Arg.string "0.0.0.0"
  @@ Arg.info ~doc:"The interface on which to listen for incoming HTTP connections."
       ~docv:"INTERFACE" [ "interface" ]
let port =
  Arg.value
  @@ Arg.opt Arg.int 8090
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
  Cmd.v info Term.(const main $ interface $ port $ backend_cap $ Prometheus_unix.opts $ Logs_cli.level ())

let () = exit @@ Cmd.eval cmd
