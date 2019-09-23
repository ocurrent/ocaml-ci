open Lwt.Infix

module Rpc = Current_rpc.Impl(Current)

let () =
  Logging.init ();
  Nocrypto_entropy_lwt.initialize () |> ignore

let webhooks = [
  "github", Current_github.input_webhook
]

let run_capnp ~engine = function
  | None -> Lwt.return_unit
  | Some public_address ->
    let config =
      Capnp_rpc_unix.Vat_config.create
        ~public_address
        ~secret_key:(`File Conf.Capnp.secret_key)
        (Capnp_rpc_unix.Network.Location.tcp ~host:"0.0.0.0" ~port:Conf.Capnp.internal_port)
    in
    let service_id = Capnp_rpc_unix.Vat_config.derived_id config "engine" in
    let restore = Capnp_rpc_lwt.Restorer.single service_id (Rpc.engine engine) in
    Capnp_rpc_unix.serve config ~restore >>= fun vat ->
    let uri = Capnp_rpc_unix.Vat.sturdy_uri vat service_id in
    let ch = open_out Conf.Capnp.cap_file in
    output_string ch (Uri.to_string uri ^ "\n");
    close_out ch;
    Logs.app (fun f -> f "Wrote capability reference to %S" Conf.Capnp.cap_file);
    Lwt.return_unit

let main config mode app capnp_address =
  let engine = Current.Engine.create ~config (Pipeline.v ~app) in
  Logging.run begin
    run_capnp ~engine capnp_address >>= fun () ->
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode ~webhooks engine;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let parse_tcp s =
  let open Astring in
  match String.cut ~sep:":" s with
  | None -> Error (`Msg "Missing :PORT in listen address")
  | Some (host, port) ->
    match String.to_int port with
    | None -> Error (`Msg "PORT must be an integer")
    | Some port ->
      Ok (Capnp_rpc_unix.Network.Location.tcp ~host ~port)

let capnp_address =
  let conv = Arg.conv (parse_tcp, Capnp_rpc_unix.Network.Location.pp) in
  Arg.value @@
  Arg.opt (Arg.some conv) None @@
  Arg.info
    ~doc:"Public address (HOST:PORT) for Cap'n Proto RPC (default: no RPC)"
    ~docv:"ADDR"
    ["capnp-address"]

let cmd =
  let doc = "Build OCaml projects on GitHub" in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $
        Current_github.App.cmdliner $ capnp_address),
  Term.info "ocaml-ci" ~doc

let () = Term.(exit @@ eval cmd)
