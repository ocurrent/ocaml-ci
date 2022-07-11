open Tyxml.Html

let render =
  Template.instance [
      p [txt "Welcome to OCaml-CI!"];
      p [txt "See ";
         a ~a:[a_href "https://github.com/apps/ocaml-ci"] [
             txt "The OCaml-CI GitHub App"
           ];
         txt " for details.";
        ];
      ul [
          li [a ~a:[a_href "/github"] [txt "Registered GitHub organisations"]];
        ]
    ]


(* let html_to_string html = *)
(*   Format.asprintf "%a" (Tyxml.Html.pp ()) html *)

let main port backend_cap =
  Lwt_main.run begin

      let open Lwt.Infix in
      let vat = Capnp_rpc_unix.client_only_vat () in
      let backend_sr = Capnp_rpc_unix.Vat.import_exn vat backend_cap in
      let backend = Backend.make backend_sr in
      Backend.ci backend >>= fun ci ->

      Dream.serve ~port
      @@ Dream.logger
      @@ Dream.origin_referrer_check
      @@ Dream.router [
             Dream.get "/github" (fun _request -> Github.list_orgs ci);
             Dream.get "/css/**" @@ Dream.static "dream-web/static/css";
             Dream.get "/" (fun _ -> Dream.html @@ render)
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
