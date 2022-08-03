open Router

let setup_logs default_level =
  Prometheus_unix.Logging.init ?default_level ();
  Dream.initialize_log ()

let main interface port backend_cap prometheus_config log_level =
  let open Lwt.Infix in
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
