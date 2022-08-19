open Tyxml.Html

(* https://github.com/aantron/dream/tree/master/example/9-error#9-error *)
let ocaml_ci_error_template _error _debug_info suggested_response =
  let status = Dream.status suggested_response in
  let code = Dream.status_to_int status
  and reason = Dream.status_to_string status in

  Dream.set_header suggested_response "Content-Type" Dream.text_html;
  Dream.set_body suggested_response
    (Template.instance [ p [ txt (Fmt.str "%d %s" code reason) ] ]);
  Lwt.return suggested_response
