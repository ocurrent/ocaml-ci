open Lwt.Infix
open Capnp_rpc_lwt

module Metrics = struct
  open Prometheus
  open Ocaml_ci

  let namespace = "ocamlci"

  let subsystem = "pipeline"

  let master =
    let help = "Number of master branches by state" in
    Gauge.v_label ~label_name:"state" ~help ~namespace ~subsystem "master_state_total"

  type stats = {
    ok : int;
    failed : int;
    active : int;
  }

  let count_repo ~owner name (acc : stats) =
    let repo = { Ocaml_ci.Repo_id.owner; name } in
    match Index.Ref_map.find_opt "refs/heads/master" (Index.get_active_refs repo) with
    | None -> acc
    | Some hash ->
      match Index.get_status ~owner ~name ~hash with
      | `Failed -> { acc with failed = acc.failed + 1 }
      | `Passed -> { acc with ok = acc.ok + 1 }
      | `Not_started | `Pending -> { acc with active = acc.active + 1 }

  let count_owner owner (acc : stats) =
    Index.Repo_set.fold (count_repo ~owner) (Index.get_active_repos ~owner) acc

  let update () =
    let owners = Index.get_active_owners () in
    let { ok; failed; active } = Index.Owner_set.fold count_owner owners { ok = 0; failed = 0; active = 0 } in
    Gauge.set (master "ok") (float_of_int ok);
    Gauge.set (master "failed") (float_of_int failed);
    Gauge.set (master "active") (float_of_int active)
end

let setup_log default_level =
  Prometheus_unix.Logging.init ?default_level ();
  Mirage_crypto_rng_unix.initialize ();
  Prometheus.CollectorRegistry.(register_pre_collect default) Metrics.update;
  match Conf.profile with
  | `Production -> Logs.info (fun f -> f "Using production configuration")
  | `Dev -> Logs.info (fun f -> f "Using dev configuration")

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let run_capnp = function
  | None -> Lwt.return (Capnp_rpc_unix.client_only_vat (), None)
  | Some public_address ->
    let config =
      Capnp_rpc_unix.Vat_config.create
        ~public_address
        ~secret_key:(`File Conf.Capnp.secret_key)
        (Capnp_rpc_unix.Network.Location.tcp ~host:"0.0.0.0" ~port:Conf.Capnp.internal_port)
    in
    let rpc_engine, rpc_engine_resolver = Capability.promise () in
    let service_id = Capnp_rpc_unix.Vat_config.derived_id config "ci" in
    let restore = Capnp_rpc_net.Restorer.single service_id rpc_engine in
    Capnp_rpc_unix.serve config ~restore >>= fun vat ->
    Capnp_rpc_unix.Cap_file.save_service vat service_id Conf.Capnp.cap_file |> or_die;
    Logs.app (fun f -> f "Wrote capability reference to %S" Conf.Capnp.cap_file);
    Lwt.return (vat, Some rpc_engine_resolver)

(* Access control policy. *)
let has_role user = function
  | `Viewer | `Monitor -> true
  | _ ->
    match Option.map Current_web.User.id user with
    | Some ( "github:talex5"
           | "github:avsm"
           | "github:kit-ty-kate"
           | "github:samoht"
           | "github:tmcgilchrist"
           | "github:dra27"
           ) -> true
    | _ -> false

let main () config mode app capnp_address github_auth submission_uri matrix : ('a, [`Msg of string]) result =
  Lwt_main.run begin
    let solver = Ocaml_ci.Solver_pool.spawn_local () in
    run_capnp capnp_address >>= fun (vat, rpc_engine_resolver) ->
    let ocluster = Option.map (Capnp_rpc_unix.Vat.import_exn vat) submission_uri in
    let engine = Current.Engine.create ~config (Pipeline.v ?ocluster ?matrix ~app ~solver) in
    rpc_engine_resolver |> Option.iter (fun r -> Capability.resolve_ok r (Api_impl.make_ci ~engine));
    let authn = Option.map Current_github.Auth.make_login_uri github_auth in
    let webhook_secret = Current_github.App.webhook_secret app in
    let has_role =
      if github_auth = None then Current_web.Site.allow_all
      else has_role
    in
    let secure_cookies = github_auth <> None in
    let routes =
      Routes.(s "webhooks" / s "github" /? nil @--> Current_github.webhook ~engine ~webhook_secret ~has_role) ::
      Routes.(s "login" /? nil @--> Current_github.Auth.login github_auth) ::
      Current_web.routes engine in
    let site = Current_web.Site.v ?authn ~has_role ~secure_cookies ~name:"ocaml-ci" routes in
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(const setup_log $ Logs_cli.level ~docs ())

let capnp_address =
  Arg.value @@
  Arg.opt (Arg.some Capnp_rpc_unix.Network.Location.cmdliner_conv) None @@
  Arg.info
    ~doc:"Public address (SCHEME:HOST:PORT) for Cap'n Proto RPC (default: no RPC)"
    ~docv:"ADDR"
    ["capnp-address"]

let submission_service =
  Arg.value @@
  Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None @@
  Arg.info
    ~doc:"The submission.cap file for the build scheduler service"
    ~docv:"FILE"
    ["submission-service"]

let cmd =
  let doc = "Build OCaml projects on GitHub" in
  let info = Cmd.info "ocaml-ci-service" ~doc in
  Cmd.v info
    Term.(term_result (const main $ setup_log $ Current.Config.cmdliner
                       $ Current_web.cmdliner $ Current_github.App.cmdliner
                       $ capnp_address $ Current_github.Auth.cmdliner
                       $ submission_service $ Matrix_current.cmdliner))

let () = exit @@ Cmd.eval cmd
