module Backend = Controller.Backend

module Middleware = struct
  let no_trailing_slash next_handler request =
    let target = "///" ^ Dream.target request in
    let path, query = Dream.split_target target in
    let path =
      Dream.from_path path |> Dream.drop_trailing_slash |> Dream.to_path
    in
    let target = path ^ if query = "" then "" else "?" ^ query in
    if Dream.target request = target then next_handler request
    else Dream.redirect request target
end

let setup_logs default_level =
  Prometheus_unix.Logging.init ?default_level ();
  Dream.initialize_log ()

let main interface port github_pipeline_cap gitlab_pipeline_cap
    prometheus_config log_level =
  Lwt_main.run
    (let () = setup_logs log_level in
     let github = Option.map Backend.make github_pipeline_cap in
     let gitlab = Option.map Backend.make gitlab_pipeline_cap in
     let web =
       Dream.serve ~interface ~port
         ~error_handler:
           (Dream.error_template View.Client_error.ocaml_ci_error_template)
       @@ Dream.logger
       @@ Middleware.no_trailing_slash
       @@ Dream.memory_sessions
       @@ Dream.flash
       @@ Router.create ~github ~gitlab
     in
     Lwt.choose (web :: Prometheus_unix.serve prometheus_config))

open Cmdliner

let interface =
  Arg.value
  @@ Arg.opt Arg.string "0.0.0.0"
  @@ Arg.info
       ~doc:"The interface on which to listen for incoming HTTP connections."
       ~docv:"INTERFACE" [ "interface" ]

let port =
  Arg.value
  @@ Arg.opt Arg.int 8090
  @@ Arg.info ~doc:"The port on which to listen for incoming HTTP connections."
       ~docv:"PORT" [ "port" ]

let backend_cap =
  Arg.value
  @@ Arg.opt (Arg.some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info
       ~doc:"The capability file giving access to the GitHub backend service."
       ~docv:"CAP" [ "backend" ]

let gitlab_backend_cap =
  Arg.value
  @@ Arg.opt (Arg.some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info
       ~doc:"The capability file giving access to the GitLab backend service."
       ~docv:"CAP" [ "gitlab-backend" ]

let cmd =
  let doc = "A web front-end for OCaml-CI" in
  let info = Cmd.info "ocaml-ci-web" ~doc in
  Cmd.v info
    Term.(
      const main
      $ interface
      $ port
      $ backend_cap
      $ gitlab_backend_cap
      $ Prometheus_unix.opts
      $ Logs_cli.level ())

let () = exit @@ Cmd.eval cmd
