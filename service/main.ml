open Lwt.Infix
open Capnp_rpc_lwt
open Ocaml_ci

module Metrics = struct
  open Prometheus

  let namespace = "ocamlci"
  let subsystem = "pipeline"

  let master =
    let help = "Number of master branches by state" in
    Gauge.v_label ~label_name:"state" ~help ~namespace ~subsystem
      "master_state_total"

  type stats = { ok : int; failed : int; active : int }

  let count_repo ~owner name (acc : stats) =
    let repo = { Repo_id.owner; name } in
    match
      Index.Ref_map.find_opt "refs/heads/master" (Index.get_active_refs repo)
    with
    | None -> acc
    | Some { Index.hash; _ } -> (
        match Index.Commit_cache.(find ~owner ~name ~hash |> get_status) with
        | `Failed -> { acc with failed = acc.failed + 1 }
        | `Passed -> { acc with ok = acc.ok + 1 }
        | `Not_started | `Pending -> { acc with active = acc.active + 1 })

  let count_owner owner (acc : stats) =
    Index.Repo_set.fold (count_repo ~owner) (Index.get_active_repos ~owner) acc

  let update () =
    let owners = Index.get_active_owners () in
    let { ok; failed; active } =
      Index.Owner_set.fold count_owner owners { ok = 0; failed = 0; active = 0 }
    in
    Gauge.set (master "ok") (float_of_int ok);
    Gauge.set (master "failed") (float_of_int failed);
    Gauge.set (master "active") (float_of_int active)
end

open Ocaml_ci_service

let setup_log level =
  Prometheus_unix.Logging.init ?default_level:level ();
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  Logs.set_level level;
  Prometheus.CollectorRegistry.(register_pre_collect default) Metrics.update;
  (match Conf.capnp_profile with
  | `Production -> Logs.info (fun f -> f "Using production capnp configuration")
  | `Dev -> Logs.info (fun f -> f "Using dev capnp configuration"));
  match Conf.platforms_profile with
  | `All -> Logs.info (fun f -> f "Testing all platforms")
  | `Minimal -> Logs.info (fun f -> f "Testing minimal platforms")

let or_die = function Ok x -> x | Error (`Msg m) -> failwith m

let check_dir x =
  Lwt.catch
    (fun () ->
      Lwt_unix.stat x >|= function
      | Unix.{ st_kind = S_DIR; _ } -> `Present
      | _ -> Fmt.failwith "Exists, but is not a directory: %S" x)
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return `Missing
      | exn -> Lwt.fail exn)

let ensure_dir path =
  check_dir path >>= function
  | `Present ->
      Logs.info (fun f -> f "Directory %s exists" path);
      Lwt.return_unit
  | `Missing ->
      Logs.info (fun f -> f "Creating %s directory" path);
      Lwt_unix.mkdir path 0o777

let run_capnp capnp_public_address capnp_listen_address =
  match (capnp_public_address, capnp_listen_address) with
  | None, None -> Lwt.return (Capnp_rpc_unix.client_only_vat (), None)
  | Some _, None ->
      Lwt.fail_invalid_arg
        "Public address for Cap'n Proto RPC can't be set without setting a \
         capnp-listen-address to listen on."
  | Some _, Some _ | None, Some _ ->
      let listen_address =
        match capnp_listen_address with
        | Some listen_address -> listen_address
        | None ->
            Capnp_rpc_unix.Network.Location.tcp ~host:"0.0.0.0"
              ~port:Conf.Capnp.internal_port
      in
      let public_address =
        match capnp_public_address with
        | None -> listen_address
        | Some public_address -> public_address
      in
      ensure_dir Conf.Capnp.cap_secrets >>= fun () ->
      let config =
        Capnp_rpc_unix.Vat_config.create ~public_address
          ~secret_key:(`File Conf.Capnp.secret_key) listen_address
      in
      let rpc_engine, rpc_engine_resolver = Capability.promise () in
      let service_id = Capnp_rpc_unix.Vat_config.derived_id config "ci" in
      let restore = Capnp_rpc_net.Restorer.single service_id rpc_engine in
      Capnp_rpc_unix.serve config ~restore >>= fun vat ->
      Capnp_rpc_unix.Cap_file.save_service vat service_id Conf.Capnp.cap_file
      |> or_die;
      Logs.app (fun f ->
          f "Wrote capability reference to %S" Conf.Capnp.cap_file);
      Lwt.return (vat, Some rpc_engine_resolver)

let main () config mode app capnp_public_address capnp_listen_address
    github_auth submission_uri solve_uri query_uri migrations :
    ('a, [ `Msg of string ]) result =
  Lwt_main.run
    (let solver = Backend_solver.v solve_uri in
     run_capnp capnp_public_address capnp_listen_address
     >>= fun (vat, rpc_engine_resolver) ->
     let ocluster =
       Option.map (Capnp_rpc_unix.Vat.import_exn vat) submission_uri
     in
     let engine =
       Current.Engine.create ~config
         (Pipeline.v ?ocluster ~app ~solver ~query_uri ~migrations)
     in
     rpc_engine_resolver
     |> Option.iter (fun r ->
            Capability.resolve_ok r (Api_impl.make_ci ~engine));
     let authn = Github.authn github_auth in
     let webhook_secret = Current_github.App.webhook_secret app in
     let has_role =
       if github_auth = None then Current_web.Site.allow_all
       else Github.has_role
     in
     let secure_cookies = github_auth <> None in
     let routes =
       Github.webhook_route ~engine ~get_job_ids:Index.get_job_ids
         ~webhook_secret
       :: Github.login_route github_auth
       :: Current_web.routes engine
     in
     let site =
       Current_web.Site.v ?authn ~has_role ~secure_cookies ~name:"ocaml-ci"
         routes
     in
     Lwt.choose [ Current.Engine.thread engine; Current_web.run ~mode site ])

(* Command-line parsing *)

open Cmdliner

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(const setup_log $ Logs_cli.level ~docs ())

let capnp_public_address =
  Arg.value
  @@ Arg.opt (Arg.some Capnp_rpc_unix.Network.Location.cmdliner_conv) None
  @@ Arg.info
       ~doc:
         "Public address (SCHEME:HOST:PORT) for Cap'n Proto RPC (default: no \
          RPC).\n\
         \          If --capnp-listen-address isn't set it will run no RPC."
       ~docv:"ADDR" [ "capnp-public-address" ]

let capnp_listen_address =
  let i =
    Arg.info ~docv:"ADDR"
      ~doc:
        "Address to listen on, e.g. $(b,unix:/run/my.socket) (default: no RPC)."
      [ "capnp-listen-address" ]
  in
  Arg.(
    value
    @@ opt (Arg.some Capnp_rpc_unix.Network.Location.cmdliner_conv) None
    @@ i)

let submission_service =
  Arg.value
  @@ Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info ~doc:"The submission.cap file for the build scheduler service"
       ~docv:"FILE" [ "submission-service" ]

let migrations =
  Arg.(
    value
    @@ opt (some dir) None
    @@ info ~docv:"MIGRATIONS_PATH"
         ~doc:
           "Specify the path to the migration directory. If no path is given \
            the migration step is ignored."
         [ "migration-path" ])

let submission_solver_service =
  Arg.value
  @@ Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info
       ~doc:
         "The submission-solve.cap file for a scheduler service which handles \
          a solver-worker. The cap file could be the same as \
          $(b,--submission-service)."
       ~docv:"FILE"
       [ "submission-solver-service" ]

let submission_query_service =
  Arg.value
  @@ Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info
       ~doc:
         "The query-solve.cap file which handles building opam variables for \
          various platforms. The cap file could be the same as \
          $(b,--submission-service) or omitted to use the local system."
       ~docv:"FILE"
       [ "submission-query-service" ]

let cmd =
  let doc = "Build OCaml projects on GitHub" in
  let info = Cmd.info "ocaml-ci-service" ~doc ~envs:Conf.cmdliner_envs in
  Cmd.v info
    Term.(
      term_result
        (const main
        $ setup_log
        $ Current.Config.cmdliner
        $ Current_web.cmdliner
        $ Current_github.App.cmdliner
        $ capnp_public_address
        $ capnp_listen_address
        $ Current_github.Auth.cmdliner
        $ submission_service
        $ submission_solver_service
        $ submission_query_service
        $ migrations))

let () = exit @@ Cmd.eval cmd
