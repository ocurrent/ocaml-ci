let solver = Ocaml_ci.Solver_pool.spawn_local ()

let () =
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Unix.putenv "PROGRESS_NO_TRUNC" "1"
  (* Logging.init () *)

let main config mode repo web =
  let open Current.Syntax in 
  let repo = Fpath.v repo in
  let p () = 
   let+ yaml = Pipeline.generate ~solver repo () in 
   Format.printf "%s%!" yaml; exit 0 
  in 
  let engine = Current.Engine.create ~config p in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:"ocaml-ci-github" (Current_web.routes engine) in
  Lwt_main.run begin
    Lwt.choose ([
      Current.Engine.thread engine;
    ] @ (if web then [ Current_web.run ~mode site ] else []))
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

let web = 
  Arg.value @@ 
  Arg.flag @@
  Arg.info 
    ~doc:"Also run the webserver so you can see what's going on"
    ~docv:"WEB"
    ["web"]

let cmd =
  let doc = "Test ocaml-ci on a local Git clone" in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ repo $ web ),
  Term.info "ocaml-ci-github" ~doc

let () = Term.(exit @@ eval cmd)