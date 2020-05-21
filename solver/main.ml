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
      let pool = Lwt_pool.create n_workers (fun () ->
          let cmd = ("", [| prog; "--worker" |]) in
          Lwt.return (Lwt_process.open_process cmd)
        ) in
      export (Service.v ~pool) ~on:Lwt_unix.stdin
    end
  | [| _prog; "--worker" |] -> Solver.main ()
  | args -> Fmt.failwith "Usage: ocaml-ci-solver (got %a)" Fmt.(array (quote string)) args
