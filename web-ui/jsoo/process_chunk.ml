module El = Brr.El
module At = Brr.At

let ansi = Ansi.create ()

let set_inner_html el html =
  Jv.set (El.to_jv el) "innerHTML" (Jv.of_string html)

let collapse_carriage_returns log_line =
  let rec last = function
    | [] -> raise (Failure "Trying to take log_line from empty list (BUG)")
    | [ s ] -> s
    | _ :: l -> last l
  in
  match log_line with
  | "" -> ""
  | log_line -> Astring.String.cuts ~sep:"\r" log_line |> last

let to_element ~line_number_id ~log_line ~code_line_class : El.t =
  ignore line_number_id;

  let colon_class_str =
    "parseInt($el.id.substring(1, $el.id.length)) >= startingLine && \
     parseInt($el.id.substring(1, $el.id.length)) <= endingLine ? 'highlight' \
     : ''"
  in
  let code = El.code [] in
  let () = set_inner_html code log_line in
  if code_line_class <> "" then El.set_class (Jstr.v code_line_class) true code;

  let span = El.span [] in
  El.set_class (Jstr.v "th") true span;
  El.set_at (Jstr.v "data-line-number") (Some (Jstr.v line_number_id)) span;

  let result = El.span ~at:At.[ id (Jstr.v line_number_id) ] [ span; code ] in
  El.set_class (Jstr.v "tr") true result;
  El.set_at (Jstr.v ":class") (Some (Jstr.v colon_class_str)) result;
  El.set_at (Jstr.v "x-on:click") (Some (Jstr.v "highlightLine")) result;
  result

let tabulate line_number data =
  let last_line_blank = ref false in
  let aux log_line =
    if !last_line_blank && log_line = "" then
      (* Squash consecutive new lines *)
      None
    else
      let is_start_of_steps_to_reproduce =
        Astring.String.is_infix ~affix:"To reproduce locally:" log_line
      in
      let is_end_of_steps_to_reproduce =
        Astring.String.is_infix ~affix:"END-REPRO-BLOCK" log_line
      in
      let code_line_class =
        if is_start_of_steps_to_reproduce then "repro-block-start"
        else if is_end_of_steps_to_reproduce then "repro-block-end"
        else ""
      in
      last_line_blank := log_line = "";
      line_number := !line_number + 1;
      let line_number_id = Printf.sprintf "L%d" !line_number in
      let line = to_element ~line_number_id ~log_line ~code_line_class in
      Some line
  in
  List.filter_map aux data

let go line_number data =
  Astring.String.(with_range ~len:(length data - 1)) data
  |> Astring.String.cuts ~sep:"\n"
  |> List.map (fun l -> collapse_carriage_returns l |> Ansi.process ansi)
  |> tabulate line_number
