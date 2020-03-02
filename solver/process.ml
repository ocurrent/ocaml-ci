open Lwt.Infix

let pp_args =
  let sep = Fmt.(const string) " " in
  Fmt.(array ~sep (quote string))

let pp_cmd f = function
  | "", args -> pp_args f args
  | bin, args -> Fmt.pf f "(%S, %a)" bin pp_args args

let pp_signal f x =
  let open Sys in
  if x = sigkill then Fmt.string f "kill"
  else if x = sigterm then Fmt.string f "term"
  else Fmt.int f x

let check_status cmd = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED x -> Fmt.failwith "%a exited with status %d" pp_cmd cmd x
  | Unix.WSIGNALED x -> Fmt.failwith  "%a failed with signal %d" pp_cmd cmd x
  | Unix.WSTOPPED x -> Fmt.failwith  "%a stopped with signal %a" pp_cmd cmd pp_signal x

let pread cmd =
  let proc = Lwt_process.open_process_in cmd in
  Lwt_io.read proc#stdout >>= fun output ->
  proc#status >|= check_status cmd >|= fun () ->
  output
