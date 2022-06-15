open Eio.Std

let setup_log default_level =
  Prometheus_unix.Logging.init ?default_level ()

module Server = Cohttp_eio.Server

let headers content_type max_age =
  Http.Header.of_list [
    ("Content-Type", content_type);
    ("Cache-Control", Printf.sprintf "public, max-age=%d;" max_age);
  ]

let static ~content_type ?(max_age=86400) body =
  let headers = headers content_type max_age in
  let headers = Http.Header.add headers "content-length" (string_of_int (String.length body)) in
  Http.Response.make ~status:`OK ~headers (), Cohttp_eio.Body.Fixed body

let crunch ?content_type ?(max_age=86400) path =
  match Static.read path with
  | None -> Server.not_found_response
  | Some body ->
    let content_type = Option.value ~default:(Magic_mime.lookup path) content_type in
    let headers = headers content_type max_age in
    let headers = Http.Header.add headers "content-length" (string_of_int (String.length body)) in
    Http.Response.make ~status:`OK ~headers (), Cohttp_eio.Body.Fixed body

let handle_request ~backend (request, _body, _addr) =
  let meth = Http.Request.meth request in
  let path = Http.Request.resource request |> Uri.pct_decode in (* XXX: pct_decode needed? *)
  Log.info (fun f -> f "HTTP %s %S" (Cohttp.Code.string_of_method meth) path);
  match meth, String.split_on_char '/' path |> List.filter ((<>) "") with
  | `GET, ([] | ["index.html"]) ->
    Server.html_response (Homepage.render ())
  | `GET, ["css"; "ansi.css"] ->
    static ~content_type:"text/css" Ansi.css
  | `GET, ["css"; _] ->
    crunch ~content_type:"text/css" path
  | meth, ("github" :: path) ->
    Github.handle ~backend meth path
  | `GET, ("badge" :: path) ->
     Badges.handle ~backend ~path
  | _ ->
    Server.not_found_response

let handle_exceptions handler req =
  try handler req
  with ex ->
    Log.warn (fun f -> f "Error handling request: %a" Fmt.exn ex);
    let headers = Http.Header.init_with "content-length" "0" in
    Http.Response.make ~headers ~status:`Internal_server_error (), Cohttp_eio.Body.Empty

let log_connection_error ex =
  Log.warn (fun f -> f "Error handling connection: %a" Fmt.exn ex)

let run_prometheus ~sw config =
  Prometheus_unix.serve config |> List.iter (fun p ->
      Fiber.fork_daemon ~sw (fun () -> Lwt_eio.Promise.await_lwt p; assert false)
    )

let main ~net () port backend_uri prometheus_config =
  Switch.run @@ fun sw ->
  let vat = Capnp_rpc_unix.client_only_vat ~sw net in
  let backend_sr = Capnp_rpc_unix.Vat.import_exn vat backend_uri in
  let backend = Backend.make ~sw backend_sr in
  Log.info (fun f -> f "Starting web server on port %d" port);
  match
    Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:128
      (`Tcp (Eio.Net.Ipaddr.V4.any, port))
  with
  | exception Unix.Unix_error(Unix.EADDRINUSE, "bind", _) ->
    Fmt.error "Web-server failed.@ Another program is already using port %d." port
  | socket ->
    let handler = Server.connection_handler (handle_exceptions @@ handle_request ~backend) in
    run_prometheus ~sw prometheus_config;
    let rec run_server () =
      Eio.Net.accept_fork ~sw socket handler ~on_error:log_connection_error;
      run_server ()
    in
    run_server ()

open Cmdliner

let setup_log =
  Term.(const setup_log $ Logs_cli.level ())

let port =
  Arg.value @@
  Arg.opt Arg.int 8090 @@
  Arg.info
    ~doc:"The port on which to listen for incoming HTTP connections."
    ~docv:"PORT"
    ["port"]

let backend_cap =
  Arg.required @@
  Arg.opt (Arg.some Capnp_rpc_unix.sturdy_uri) None @@
  Arg.info
    ~doc:"The capability file giving access to the CI backend service."
    ~docv:"CAP"
    ["backend"]

let cmd ~net =
  let doc = "A web front-end for OCaml-CI" in
  let info = Cmd.info "ocaml-ci-web" ~doc in
  Cmd.v info Term.(const (main ~net) $ setup_log $ port $ backend_cap $ Prometheus_unix.opts)

let () =
  exit @@ Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  Cmd.eval_result (cmd ~net:env#net)
