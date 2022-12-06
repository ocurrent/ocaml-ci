open Lwt.Infix

let n_workers = 20

let pp_timestamp f x =
  let open Unix in
  let tm = localtime x in
  Fmt.pf f "%04d-%02d-%02d %02d:%02d.%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let reporter =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stderr
      ("%a %a %a @[" ^^ fmt ^^ "@]@.")
      pp_timestamp (Unix.gettimeofday ())
      Fmt.(styled `Magenta string)
      (Printf.sprintf "%14s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report }

let setup_log level =
  Logs.set_level level;
  Logs.set_reporter reporter

let export service ~on:socket =
  let restore =
    Capnp_rpc_net.Restorer.single
      (Capnp_rpc_net.Restorer.Id.public "solver")
      service
  in
  let switch = Lwt_switch.create () in
  let stdin =
    Capnp_rpc_unix.Unix_flow.connect socket
    |> Capnp_rpc_net.Endpoint.of_flow
         (module Capnp_rpc_unix.Unix_flow)
         ~peer_id:Capnp_rpc_net.Auth.Digest.insecure ~switch
  in
  let (_ : Capnp_rpc_unix.CapTP.t) =
    Capnp_rpc_unix.CapTP.connect ~restore stdin
  in
  let crashed, set_crashed = Lwt.wait () in
  Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
      Lwt.wakeup_exn set_crashed (Failure "Capnp switch turned off");
      Lwt.return_unit)
  >>= fun () -> crashed

let main () sockpath = function
  | None ->
      let socket =
        match sockpath with
        | Some path ->
            let sock = Unix.(socket PF_UNIX SOCK_STREAM 0) in
            Unix.connect sock (ADDR_UNIX path);
            Lwt_unix.of_unix_file_descr sock
        | None -> Lwt_unix.stdin
      in
      Lwt_main.run
        (let create_worker hash =
           let cmd =
             ( "",
               [| Sys.argv.(0); "--worker"; Git_unix.Store.Hash.to_hex hash |]
             )
           in
           Lwt_process.open_process cmd
         in
         Service.v ~n_workers ~create_worker >>= fun service ->
         export service ~on:socket)
  | Some hash -> Solver.main (Git_unix.Store.Hash.of_hex hash)

(* Command-line parsing *)

open Cmdliner

let setup_log = Term.(const setup_log $ Logs_cli.level ())

let worker_hash =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The hash of the worker." ~docv:"HASH" [ "worker" ]

let sockpath =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info
       ~doc:
         "The UNIX domain socket path to read requests from, if not provided \
          will use stdin."
       ~docv:"SOCKPATH" [ "sockpath" ]

let cmd =
  let doc = "Solver for ocaml-ci" in
  let info = Cmd.info "solver" ~doc in
  Cmd.v info Term.(const main $ setup_log $ sockpath $ worker_hash)

let () = exit @@ Cmd.eval cmd
