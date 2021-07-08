let spawn_local ?solver_dir () : Ocaml_ci_api.Solver.t =
  let p, c = Unix.(socketpair PF_UNIX SOCK_STREAM 0 ~cloexec:true) in
  Unix.clear_close_on_exec c;
  let solver_dir =
    match solver_dir with
    | None -> Fpath.to_string (Current.state_dir "solver")
    | Some x -> x
  in
  let cmd = ("", [| "ocaml-ci-solver" |]) in
  let _child = Lwt_process.open_process_none ~cwd:solver_dir ~stdin:(`FD_move c) cmd in
  let switch = Lwt_switch.create () in
  let p =
    Lwt_unix.of_unix_file_descr p
    |> Capnp_rpc_unix.Unix_flow.connect ~switch
    |> Capnp_rpc_net.Endpoint.of_flow
         (module Capnp_rpc_unix.Unix_flow)
         ~peer_id:Capnp_rpc_net.Auth.Digest.insecure ~switch
  in
  let conn = Capnp_rpc_unix.CapTP.connect ~restore:Capnp_rpc_net.Restorer.none p in
  let solver =
    Capnp_rpc_unix.CapTP.bootstrap conn (Capnp_rpc_net.Restorer.Id.public "solver")
  in
  solver
  |> Capnp_rpc_lwt.Capability.when_broken (fun ex ->
         Fmt.failwith "Solver process failed: %a" Capnp_rpc.Exception.pp ex);
  solver
