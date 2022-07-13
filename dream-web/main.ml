let main port backend_cap =
  Lwt_main.run begin
      let open Lwt.Infix in

      Backend.Make.ci backend_cap >>= fun ci ->

      Dream.serve ~port
      @@ Dream.logger
      @@ Dream.origin_referrer_check
      @@ Dream.router [
             Dream.get "/github" (fun _request -> Controller.Github.handle ~path:[] ci);
             Dream.get "/css/**" @@ Dream.static "dream-web/static/css";
             Dream.get "/" (fun _ -> Dream.html @@ Controller.Index.render)
           ]
    end


open Cmdliner

let port =
  Arg.value @@
    Arg.opt Arg.int 8090 @@
      Arg.info
        ~doc:"The port on which to listen for incoming HTTP connections."
        ~docv:"PORT"
        ["port"]

let backend_cap =
  Arg.required @@
    Arg.opt (Arg.some Capnp_rpc_unix.sturdy_uri) None @@
      Arg.info
        ~doc:"The capability file giving access to the CI backend service."
        ~docv:"CAP"
        ["backend"]

let cmd =
  let doc = "A web front-end for OCaml-CI" in
  let info = Cmd.info "ocaml-ci-web" ~doc in
  Cmd.v info Term.(const main $ port $ backend_cap)

let () = exit @@ Cmd.eval cmd
