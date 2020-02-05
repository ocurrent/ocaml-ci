let path_from_uri uri =
  String.split_on_char '/' (Uri.path uri) |>
  List.filter (fun s -> not (String.equal s ""))

let callback _conn req body =
  let headers = Cohttp.Request.headers req in
  let meth = Cohttp.Request.meth req in
  let uri = Cohttp.Request.uri req in
  let uri = Uri.with_host uri None in
  let uri = match path_from_uri uri with
    | "webhooks"::_ -> Uri.with_port uri (Some 8082) (* service *)
    | _ -> Uri.with_port uri (Some 8081) (* web-ui *)
  in
  Cohttp_lwt_unix.Client.call ~headers ~body meth uri

let () =
  Lwt_main.run begin
    Cohttp_lwt_unix.Server.create
      ~on_exn:(fun e -> prerr_endline Printexc.(get_backtrace () ^ to_string e))
      ~mode:(`TCP (`Port 8080))
      (Cohttp_lwt_unix.Server.make ~callback ())
  end
