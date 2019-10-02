let src = Logs.Src.create "ocaml_ci_web" ~doc:"ocaml-ci web interface"
include (val Logs.src_log src : Logs.LOG)
