open Capnp_rpc_lwt
open Lwt.Infix

let retry_delay = 5.0
(* Time to wait after a failed connection before retrying. *)

module Metrics = struct
  open Prometheus

  let namespace = "ocamlci"

  let subsystem = "web"

  let backend_down =
    let help = "Whether the connection to the backend is down" in
    Gauge.v ~help ~namespace ~subsystem "backend_down"
end

type t = {
  sr : Ocaml_ci_api.Raw.Client.CI.t Sturdy_ref.t;
  mutable ci : Ocaml_ci_api.Client.CI.t Lwt.t;
  mutable last_failed : float;
}

let rec reconnect t =
  Prometheus.Gauge.set Metrics.backend_down 1.0;
  let now = Unix.gettimeofday () in
  let delay = t.last_failed +. retry_delay -. now in
  t.last_failed <- now;
  Lwt.async (fun () ->
      (if delay > 0.0 then Lwt_unix.sleep delay else Lwt.return_unit) >>= fun () ->
      Log.info (fun f -> f "Reconnecting to backend");
      let ci =
        try Sturdy_ref.connect_exn t.sr with ex -> Lwt.fail ex
        (* (just in case) *)
      in
      t.ci <- ci;
      monitor t;
      Lwt.return_unit)

and monitor t =
  Lwt.on_any t.ci
    (fun ci ->
      (* Connected OK - now watch for failure. *)
      Prometheus.Gauge.set Metrics.backend_down 0.0;
      ci
      |> Capability.when_broken @@ fun ex ->
         Log.warn (fun f -> f "Lost connection to backend: %a" Capnp_rpc.Exception.pp ex);
         reconnect t)
    (fun ex ->
      (* Failed to connect. *)
      Log.warn (fun f -> f "Failed to connect to backend: %a" Fmt.exn ex);
      reconnect t)

let make sr =
  let ci = Sturdy_ref.connect_exn sr in
  let t = { sr; ci; last_failed = 0.0 } in
  Prometheus.Gauge.set Metrics.backend_down 1.0;
  monitor t;
  t

let ci t = t.ci
