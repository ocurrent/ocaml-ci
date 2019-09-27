open Lwt.Infix

let () =
  Logging.init ();
  Nocrypto_entropy_lwt.initialize () |> ignore;
  match Conf.profile with
  | `Production -> Logs.info (fun f -> f "Using production configuration")
  | `Dev -> Logs.info (fun f -> f "Using dev configuration")

let webhooks = [
  "github", Current_github.input_webhook
]

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let run_capnp ~engine = function
  | None -> Lwt.return_unit
  | Some public_address ->
    let config =
      Capnp_rpc_unix.Vat_config.create
        ~public_address
        ~secret_key:(`File Conf.Capnp.secret_key)
        (Capnp_rpc_unix.Network.Location.tcp ~host:"0.0.0.0" ~port:Conf.Capnp.internal_port)
    in
    let service_id = Capnp_rpc_unix.Vat_config.derived_id config "ci" in
    let restore = Capnp_rpc_lwt.Restorer.single service_id (Api_impl.make_ci ~engine) in
    Capnp_rpc_unix.serve config ~restore >>= fun vat ->
    Capnp_rpc_unix.Cap_file.save_service vat service_id Conf.Capnp.cap_file |> or_die;
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

let capnp_address =
  Arg.value @@
  Arg.opt (Arg.some Capnp_rpc_unix.Network.Location.cmdliner_conv) None @@
  Arg.info
    ~doc:"Public address (SCHEME:HOST:PORT) for Cap'n Proto RPC (default: no RPC)"
    ~docv:"ADDR"
    ["capnp-address"]

let cmd =
  let doc = "Build OCaml projects on GitHub" in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $
        Current_github.App.cmdliner $ capnp_address),
  Term.info "ocaml-ci" ~doc

let () = Term.(exit @@ eval cmd)
