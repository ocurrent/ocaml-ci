module Client = Ocaml_ci_api.Client
open Tyxml.Html

let cancel_success_message success =
  let format_job_info ji =
    li [ span [ txt @@ Printf.sprintf "Cancelling job: %s" ji.Client.variant ] ]
  in
  match success with
  | [] -> div [ span [ txt "No jobs were cancelled." ] ]
  | success -> ul (List.map format_job_info success)

let success_message_v1 action jis =
  let empty_message, prefix =
    match action with
    | `Cancel -> ("No jobs were cancelled.", "Cancelling")
    | `Rebuild -> ("No jobs were rebuilt.", "Rebuilding")
  in
  match jis with
  | [] -> [ (`Success, empty_message) ]
  | jis ->
      let trimmed = List.filteri (fun i _ -> i < 5) jis in
      let message =
        Astring.String.concat ~sep:", "
          (List.map (fun ji -> ji.Client.variant) trimmed)
      in
      if List.compare_length_with jis 5 > 0 then
        [
          ( `Success,
            Astring.String.concat [ prefix; " many: "; message; " ..." ] );
        ]
      else [ (`Success, Astring.String.concat [ prefix; ": "; message ]) ]

let cancel_fail_message = function
  | n when n <= 0 -> div []
  | 1 ->
      div
        [
          span
            [ txt "1 job could not be cancelled. Check logs for more detail." ];
        ]
  | n ->
      div
        [
          span
            [
              txt
              @@ Printf.sprintf
                   "%d jobs could not be cancelled. Check logs for more detail."
                   n;
            ];
        ]

let cancel_fail_message_v1 : int -> ([> `Fail ] * uri) list_wrap = function
  | n when n <= 0 -> []
  | 1 ->
      [ (`Fail, "1 job could not be cancelled. Check logs for more detail.") ]
  | n ->
      [
        ( `Fail,
          Printf.sprintf
            "%d jobs could not be cancelled. Check logs for more detail." n );
      ]

let rebuild_success_message success =
  let format_job_info ji =
    li [ span [ txt @@ Printf.sprintf "Rebuilding job: %s" ji.Client.variant ] ]
  in
  match success with
  | [] -> div [ span [ txt "No jobs were rebuilt." ] ]
  | success -> ul (List.map format_job_info success)

let rebuild_fail_message = function
  | n when n <= 0 -> div []
  | 1 ->
      div
        [
          span [ txt "1 job could not be rebuilt. Check logs for more detail." ];
        ]
  | n ->
      div
        [
          span
            [
              txt
              @@ Printf.sprintf
                   "%d jobs could not be rebuilt. Check logs for more detail." n;
            ];
        ]

let rebuild_fail_message_v1 = function
  | n when n <= 0 -> []
  | 1 -> [ (`Fail, "1 job could not be rebuilt. Check logs for more detail.") ]
  | n ->
      [
        ( `Fail,
          Printf.sprintf
            "%d jobs could not be rebuilt. Check logs for more detail." n );
      ]
