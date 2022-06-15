open Eio.Std
open Capnp_rpc_lwt

let retry_delay = 5.0   (* Time to wait after a failed connection before retrying. *)

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
  mutable ci : (Ocaml_ci_api.Client.CI.t, Capnp_rpc.Exception.t) result Promise.t;
  mutable last_attempt : float;
}

let rate_limit t =
  let now = Unix.gettimeofday () in
  let delay = t.last_attempt +. retry_delay -. now in
  if delay > 0.0 then Eio_unix.sleep delay;
  t.last_attempt <- Unix.gettimeofday ()

let await_broken cap =
  let p, r = Promise.create () in
  Capability.when_broken (Promise.resolve r) cap;
  Promise.await p

let rec connect t resolver =
  Prometheus.Gauge.set Metrics.backend_down 1.0;
  rate_limit t;
  Log.info (fun f -> f "Connecting to backend");
  let ci = Sturdy_ref.connect t.sr in
  Promise.resolve resolver ci;
  match ci with
  | Error ex ->
    Log.warn (fun f -> f "Failed to connect to backend: %a" Capnp_rpc.Exception.pp ex);
    reconnect t
  | Ok ci ->
    (* Connected OK - now watch for failure. *)
    Prometheus.Gauge.set Metrics.backend_down 0.0;
    let ex = await_broken ci in
    Log.warn (fun f -> f "Lost connection to backend: %a" Capnp_rpc.Exception.pp ex);
    reconnect t
and reconnect t =
  let ci, r = Promise.create () in
  t.ci <- ci;
  connect t r

let make ~sw sr =
  let ci, r = Promise.create () in
  let t = { sr; ci; last_attempt = 0.0 } in
  Fiber.fork_daemon ~sw (fun () -> connect t r);
  t

let ci t =
  match Promise.await t.ci with
  | Ok x -> x
  | Error e -> Fmt.failwith "%a" Capnp_rpc.Exception.pp e
