(* Windows: don't ever try to create a filesystem entry and reuse the
   same name for the Unix-domain socket. *)

let temp_file_name prefix suffix =
  let temp_dir = Filename.get_temp_dir_name () in
  let rnd = Random.(State.bits (get_state ())) land 0xFFFFFF in
  Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

let spawn_local ?solver_dir () : Ocaml_ci_api.Solver.t =
  let name = temp_file_name "ocaml-ci-solver-" ".sock" in
  Logs.info (fun f -> f "Setting up ocaml-ci-solver %s…" name);
  let listener = Unix.socket ~cloexec:true PF_UNIX SOCK_STREAM 0 in
  (try Unix.unlink name with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  Unix.bind listener (ADDR_UNIX name);
  Unix.listen listener 1;
  let solver_dir =
    match solver_dir with
    | None -> Fpath.to_string (Current.state_dir "solver")
    | Some x -> x
  in
  let cmd = ("", [| "solver-service"; "--sockpath"; name |]) in
  let _child =
    Lwt_process.open_process_none ~cwd:solver_dir ~stdin:`Close cmd
  in
  let switch = Lwt_switch.create () in
  let p, _ =
    match Unix.select [ listener ] [] [] 1. with
    | [ listener' ], [], [] when listener = listener' ->
        Unix.accept ~cloexec:true listener
    | _ -> failwith "Solver process didn't start correctly"
    | exception (Unix.Unix_error (Unix.EINTR, _, _) as exn) ->
        prerr_endline "Solver process didn't start correctly";
        raise exn
  in
  Unix.close listener;
  Unix.unlink name;
  let p =
    Lwt_unix.of_unix_file_descr p
    |> Capnp_rpc_unix.Unix_flow.connect ~switch
    |> Capnp_rpc_net.Endpoint.of_flow
         (module Capnp_rpc_unix.Unix_flow)
         ~peer_id:Capnp_rpc_net.Auth.Digest.insecure ~switch
  in
  let conn =
    Capnp_rpc_unix.CapTP.connect ~restore:Capnp_rpc_net.Restorer.none p
  in
  let solver =
    Capnp_rpc_unix.CapTP.bootstrap conn
      (Capnp_rpc_net.Restorer.Id.public "solver")
  in
  solver
  |> Capnp_rpc_lwt.Capability.when_broken (fun ex ->
         Fmt.failwith "Solver process failed: %a" Capnp_rpc.Exception.pp ex);
  solver
