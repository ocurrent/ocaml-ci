let () =
  Logging.init ();
  Nocrypto_entropy_lwt.initialize () |> ignore

let webhooks = [
  "github", Current_github.input_webhook
]

let main config mode app =
  let engine = Current.Engine.create ~config (Pipeline.v ~app) in
  Logging.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode ~webhooks engine;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "Build OCaml projects on GitHub" in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ Current_github.App.cmdliner),
  Term.info "ocaml-ci" ~doc

let () = Term.(exit @@ eval cmd)
