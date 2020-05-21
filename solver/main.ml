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

let pp_status f = function
  | Unix.WEXITED x -> Fmt.pf f "exited with status %d" x
  | Unix.WSIGNALED x -> Fmt.pf f "failed with signal %d" x
  | Unix.WSTOPPED x -> Fmt.pf f "stopped with signal %d" x

let validate (worker : Lwt_process.process) =
  match Lwt.state worker#status with
  | Lwt.Sleep -> Lwt.return true
  | Lwt.Fail ex -> Lwt.fail ex
  | Lwt.Return status ->
    Format.eprintf "Worker %d is dead (%a) - removing from pool@." worker#pid pp_status status;
    Lwt.return false

let () =
  match Sys.argv with
  | [| prog |] ->
    Lwt_main.run begin
      let pool = Lwt_pool.create n_workers ~validate (fun () ->
          let cmd = ("", [| prog; "--worker" |]) in
          Lwt.return (Lwt_process.open_process cmd)
        ) in
      export (Service.v ~pool) ~on:Lwt_unix.stdin
    end
  | [| _prog; "--worker" |] -> Solver.main ()
  | args -> Fmt.failwith "Usage: ocaml-ci-solver (got %a)" Fmt.(array (quote string)) args
