(* Utility program for testing the CI pipeline on a local repository. *)

let solver = Ocaml_ci.Solver_pool.spawn_local ()

let () =
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Unix.putenv "PROGRESS_NO_TRUNC" "1";
  Prometheus_unix.Logging.init ()

let main config mode repo : ('a, [`Msg of string]) result =
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

let repo =
  Arg.required @@
  Arg.pos 0 Arg.(some dir) None @@
  Arg.info
    ~doc:"The directory containing the .git subdirectory."
    ~docv:"DIR"
    []

let cmd =
  let doc = "Test ocaml-ci on a local Git clone" in
  Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ repo)),
  Term.info "ocaml-ci-local" ~doc

let () = Term.(exit @@ eval cmd)
