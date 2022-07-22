(* Utility program for testing the CI pipeline on a local repository. *)

let setup_log default_level =
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Unix.putenv "PROGRESS_NO_TRUNC" "1";
  Prometheus_unix.Logging.init ?default_level ()

let main () config mode repo : ('a, [`Msg of string]) result =
  let open Ocaml_ci_service in
  let solver = Ocaml_ci.Solver_pool.spawn_local () in
  let repo = Current_git.Local.v (Fpath.v repo) in
  let engine = Current.Engine.create ~config (Pipeline.local_test ~solver repo) in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:"ocaml-ci-local" (Current_web.routes engine) in
  Lwt_main.run begin
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

let repo =
  Arg.required @@
  Arg.pos 0 Arg.(some dir) None @@
  Arg.info
    ~doc:"The directory containing the .git subdirectory."
    ~docv:"DIR"
    []

let cmd =
  let doc = "Test ocaml-ci on a local Git clone" in
  let info = Cmd.info "ocaml-ci-local" ~doc ~envs:Ocaml_ci_service.Conf.cmdliner_envs in
  Cmd.v info Term.(term_result (const main $ setup_log $ Current.Config.cmdliner $ Current_web.cmdliner $ repo))

let () = exit @@ Cmd.eval cmd
