open Lwt.Infix
open Capnp_rpc_lwt

module Log = struct
  module X = Raw.Client.Log
  type t = X.t Capability.t

  let pp_timestamp f x =
    let open Unix in
    let tm = gmtime x in
    Fmt.pf f "%04d-%02d-%02d %02d:%02d.%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
      tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

  let write t msg =
    let open X.Write in
    let message_size = 150 + String.length msg in
    let request, params = Capability.Request.create ~message_size Params.init_pointer in
    Params.msg_set params msg;
    Capability.call_for_unit_exn t method_id request

  let info t fmt =
    let now = Unix.gettimeofday () in
    let k msg =
      let thread = write t msg in
      Lwt.on_failure thread (fun ex -> Format.eprintf "Log.info(%S) failed: %a@." msg Fmt.exn ex)
    in
    Fmt.kstr k ("%a [INFO] " ^^ fmt ^^ "@.") pp_timestamp now
end

module X = Raw.Client.Solver

type t = X.t Capability.t

let solve t ~log reqs =
  let open X.Solve in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.request_set params (Worker.Solve_request.to_yojson reqs |> Yojson.Safe.to_string);
  Params.log_set params (Some log);
  Capability.call_for_value_exn t method_id request >|= Results.response_get >|= fun json ->
  match Worker.Solve_response.of_yojson (Yojson.Safe.from_string json) with
  | Ok x -> x
  | Error ex -> failwith ex
