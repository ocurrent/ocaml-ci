let ansi = Ansi.create ()

let collapse_carriage_returns log_line =
  let rec last = function
    | [] -> raise (Failure "Trying to take log_line from empty list (BUG)")
    | [ s ] -> s
    | _ :: l -> last l
  in
  match log_line with
  | "" -> ""
  | log_line -> Astring.String.cuts ~sep:"\r" log_line |> last

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
      let line =
        let open Tyxml.Html in
        Fmt.str "%a" (pp_elt ())
          (span
             ~a:
               [
                 a_class [ "tr" ];
                 Tyxml.Html.Unsafe.string_attrib ":class"
                   "parseInt($el.id.substring(1, $el.id.length)) >= \
                    startingLine && parseInt($el.id.substring(1, \
                    $el.id.length)) <= endingLine ? 'highlight' : ''";
                 Tyxml.Html.Unsafe.string_attrib "@click" "highlightLine";
                 a_id line_number_id;
               ]
             [
               span
                 ~a:
                   [
                     a_class [ "th" ]; a_user_data "line-number" line_number_id;
                   ]
                 [];
               code
                 ~a:
                   [
                     a_class [ code_line_class ];
                     a_user_data "line-number" line_number_id;
                   ]
                 [ Unsafe.data log_line ];
             ])
      in
      Some line
  in
  List.filter_map aux data |> String.concat "\n"

let go line_number data =
  Astring.String.(with_range ~len:(length data - 1)) data
  |> Astring.String.cuts ~sep:"\n"
  |> List.map (fun l -> collapse_carriage_returns l |> Ansi.process ansi)
  |> tabulate line_number
