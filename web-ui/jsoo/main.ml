module WebSocket = Brr_io.Websocket
module Ev = Brr.Ev
module El = Brr.El
module Document = Brr.Document
module Window = Brr.Window

let regexp_left_paren = Re.Str.regexp_string "("
let regexp_right_paren = Re.Str.regexp_string ")"
let encoded_left_paren = "%28"
let encoded_right_paren = "%29"

let encode_parens s : Jstr.t =
  let s' = Jstr.to_string s in
  let s_lp_encoded =
    Re.Str.global_replace regexp_left_paren encoded_left_paren s'
  in
  let s_rp_encoded =
    Re.Str.global_replace regexp_right_paren encoded_right_paren s_lp_encoded
  in
  Jstr.of_string s_rp_encoded

let inject_log_lines first_line_repro_block last_line_repro_block data =
  match Document.find_el_by_id Brr.G.document (Jstr.of_string "logs-pre") with
  | None -> assert false
  | Some scroller ->
      El.append_children scroller data;
      if !last_line_repro_block <> 0 then
        Steps_to_reproduce_build.go !first_line_repro_block
          !last_line_repro_block;
      ()

let ws_path window =
  let location = Brr.Window.location window in
  let pathname = Brr.Uri.path location in
  let port = Brr.Uri.port location in
  let hostname = Brr.Uri.host location in
  match port with
  | None ->
      Jstr.concat ~sep:(Jstr.of_string "/")
        [
          Jstr.of_string "ws:/";
          hostname;
          encode_parens @@ Jstr.append (Jstr.of_string "ws") pathname;
        ]
  | Some port ->
      Jstr.concat ~sep:(Jstr.of_string "/")
        [
          Jstr.of_string "ws:/";
          Jstr.concat ~sep:(Jstr.of_string ":") [ hostname; Jstr.of_int port ];
          encode_parens @@ Jstr.append (Jstr.of_string "ws") pathname;
        ]

(* It looks like parens are not being percent encoded - will file an issue with Brr. Then the code below should work
   Brr.Uri.with_uri ~scheme:(Jstr.of_string "ws")
   ~path:(encode_parens @@ Jstr.append (Jstr.of_string "ws") pathname)
   location *)

let fetch_logs =
  let line_number = ref 0 in
  let first_line_repro_block = ref 0 in
  let last_line_repro_block = ref 0 in

  let window = Brr.G.window in
  (* this will throw an exception if the encoding fails *)
  let ws_path = ws_path window in
  let socket = WebSocket.create ws_path in
  let target = WebSocket.as_target socket in
  let (_result : Ev.listener) =
    Ev.listen Brr_io.Message.Ev.message
      (fun e ->
        let data = Brr_io.Message.Ev.data (Ev.as_type e) in
        inject_log_lines first_line_repro_block last_line_repro_block
        @@ Process_chunk.go line_number first_line_repro_block
             last_line_repro_block (Jstr.to_string data))
      target
  in
  ()

let () = ignore fetch_logs
