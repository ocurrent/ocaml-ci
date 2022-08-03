val setup_logs : Logs.level option -> unit

val main :
  string -> int -> Uri.t -> Prometheus_unix.config -> Logs.level option -> unit
