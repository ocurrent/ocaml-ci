open Lwt.Infix

let n_workers = 20

let export service ~on:socket =
  let restore = Capnp_rpc_net.Restorer.single (Capnp_rpc_net.Restorer.Id.public "solver") service in
  let switch = Lwt_switch.create () in
  let stdin = Capnp_rpc_unix.Unix_flow.connect socket
              |> Capnp_rpc_net.Endpoint.of_flow (module Capnp_rpc_unix.Unix_flow)
                ~peer_id:Capnp_rpc_net.Auth.Digest.insecure
                ~switch
  in
  let _ : Capnp_rpc_unix.CapTP.t = Capnp_rpc_unix.CapTP.connect ~restore stdin in
  fst (Lwt.wait ())

let () =
  match Sys.argv with
  | [| prog |] ->
    Lwt_main.run begin
      let create_worker hash =
        let cmd = ("", [| prog; "--worker"; Git_unix.Store.Hash.to_hex hash |]) in
        Lwt_process.open_process cmd
      in
      Service.v ~n_workers ~create_worker >>= fun service ->
      export service ~on:Lwt_unix.stdin
    end
  | [| _prog; "--worker"; hash |] -> Solver.main (Git_unix.Store.Hash.of_hex hash)
  | args -> Fmt.failwith "Usage: ocaml-ci-solver (got %a)" Fmt.(array (quote string)) args
