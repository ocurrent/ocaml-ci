open Lwt.Infix
open Astring

let () =
  Logging.init ()

module Server = Cohttp_lwt_unix.Server

let errorf fmt =
  fmt |> Fmt.kstrf @@ fun msg -> Error (`Msg msg)

let normal_response x =
  x >|= fun x -> `Response x

let handle_request ~backend _conn request _body =
  let meth = Cohttp.Request.meth request in
  let uri = Cohttp.Request.uri request in
  let path = Uri.path uri in
  Log.info (fun f -> f "HTTP %s %S" (Cohttp.Code.string_of_method meth) path);
  match meth, String.cuts ~sep:"/" ~empty:false path with
  | `GET, ([] | ["index.html"]) ->
    let body = Homepage.render () in
    Server.respond_string ~status:`OK ~body () |> normal_response
  | `GET, ["css"; "style.css"] ->
    Style.get () |> normal_response
  | meth, ("github" :: path) ->
    Github.handle ~backend ~meth path
  | `GET, ("badge" :: path) ->
     Badges.handle ~backend ~path
  | _ ->
    Server.respond_not_found () |> normal_response

let pp_mode f mode =
  Sexplib.Sexp.pp_hum f (Conduit_lwt_unix.sexp_of_server mode)

let main port backend_uri prometheus_config =
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
  Term.(const main $ port $ backend_cap $ Prometheus_unix.opts),
  Term.info "ocaml-ci-web" ~doc

let () = Term.(exit @@ eval cmd)
