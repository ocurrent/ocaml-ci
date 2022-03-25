open Lwt.Infix
open Astring

let setup_log default_level =
  Prometheus_unix.Logging.init ?default_level ()

module Server = Cohttp_lwt_unix.Server

let normal_response x =
  x >|= fun x -> `Response x

let headers content_type max_age =
  Cohttp.Header.of_list [
    ("Content-Type", content_type);
    ("Cache-Control", Printf.sprintf "public, max-age=%d;" max_age);
  ]

let static ~content_type ?(max_age=86400) body =
  let headers = headers content_type max_age in
  Server.respond_string ~status:`OK ~headers ~body ()

let crunch ?content_type ?(max_age=86400) path =
  match Static.read path with
  | None -> Server.respond_not_found ()
  | Some body ->
    let content_type = Option.value ~default:(Magic_mime.lookup path) content_type in
    let headers = headers content_type max_age in
    Server.respond_string ~status:`OK ~headers ~body ()

let handle_request ~backend _conn request _body =
  let meth = Cohttp.Request.meth request in
  let uri = Cohttp.Request.uri request in
  let path = Uri.(uri |> path |> pct_decode) in
  Log.info (fun f -> f "HTTP %s %S" (Cohttp.Code.string_of_method meth) path);
  match meth, String.cuts ~sep:"/" ~empty:false path with
  | `GET, ([] | ["index.html"]) ->
    let body = Homepage.render () in
    let headers = Cohttp.Header.init_with "Content-Type" "text/html; charset=utf-8" in
    Server.respond_string ~status:`OK ~headers ~body () |> normal_response
  | `GET, ["css"; "ansi.css"] ->
    static ~content_type:"text/css" Ansi.css |> normal_response
  | `GET, ["css"; _] ->
    crunch ~content_type:"text/css" path |> normal_response
  | meth, ("github" :: path) ->
    Github.handle ~backend ~meth path
  | meth, ("gitlab" :: path) -> (* TODO Conditionally enable this based off Git Forges known about. *)
    Gitlab.handle ~backend ~meth path
  | `GET, ("badge" :: path) ->
     Badges.handle ~backend ~path
  | _ ->
    Server.respond_not_found () |> normal_response

let pp_mode f mode =
  Sexplib.Sexp.pp_hum f (Conduit_lwt_unix.sexp_of_server mode)

let main () port backend_uri prometheus_config =
  Lwt_main.run begin
    let vat = Capnp_rpc_unix.client_only_vat () in
    let backend_sr = Capnp_rpc_unix.Vat.import_exn vat backend_uri in
    let backend = Backend.make backend_sr in
    let config = Server.make_response_action ~callback:(handle_request ~backend) () in
    let mode = `TCP (`Port port) in
    Log.info (fun f -> f "Starting web server: %a" pp_mode mode);
    let web =
      Lwt.try_bind
        (fun () -> Server.create ~mode config)
        (fun () -> failwith "Web-server stopped!")
        (function
          | Unix.Unix_error(Unix.EADDRINUSE, "bind", _) ->
            Fmt.failwith "Web-server failed.@ Another program is already using this port %a." pp_mode mode
          | ex -> Lwt.fail ex
        )
    in
    Lwt.choose (web :: Prometheus_unix.serve prometheus_config)
  end

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

let cmd =
  let doc = "A web front-end for OCaml-CI" in
  let info = Cmd.info "ocaml-ci-web" ~doc in
  Cmd.v info Term.(const main $ setup_log $ port $ backend_cap $ Prometheus_unix.opts)

let () = exit @@ Cmd.eval cmd
