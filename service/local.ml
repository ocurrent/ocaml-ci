(* Utility program for testing the CI pipeline on a local repository. *)

let setup_log style_renderer default_level =
  if not Sys.win32 then Unix.putenv "DOCKER_BUILDKIT" "1";
  Unix.putenv "PROGRESS_NO_TRUNC" "1";
  Prometheus_unix.Logging.init ?default_level ();
  Fmt_tty.setup_std_outputs ?style_renderer ();
  ()

let main () config mode repo solve_uri : ('a, [ `Msg of string ]) result =
  let open Ocaml_ci_service in
  let solver = Ocaml_ci.Backend_solver.v solve_uri in
  let repo = Current_git.Local.v (Fpath.v repo) in
  let engine =
    Current.Engine.create ~config (Pipeline.local_test ~solver repo)
  in
  let site =
    Current_web.Site.(v ~has_role:allow_all)
      ~name:"ocaml-ci-local"
      (Current_web.routes engine)
  in
  Lwt_main.run
    (Lwt.choose [ Current.Engine.thread engine; Current_web.run ~mode site ])

(* Command-line parsing *)

open Cmdliner

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(
    const setup_log $ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ())

let repo =
  Arg.required
  @@ Arg.pos 0 Arg.(some dir) None
  @@ Arg.info ~doc:"The directory containing the .git subdirectory." ~docv:"DIR"
       []

let submission_solver_service =
  Arg.value
  @@ Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info
       ~doc:
         "The submission-solve.cap file for a scheduler service which handles \
          a solver-worker."
       ~docv:"FILE"
       [ "submission-solver-service" ]

let cmd =
  let doc = "Test ocaml-ci on a local Git clone" in
  let info =
    Cmd.info "ocaml-ci-local" ~doc ~envs:Ocaml_ci_service.Conf.cmdliner_envs
  in
  Cmd.v info
    Term.(
      term_result
        (const main
        $ setup_log
        $ Current.Config.cmdliner
        $ Current_web.cmdliner
        $ repo
        $ submission_solver_service))

let () = exit @@ Cmd.eval cmd
