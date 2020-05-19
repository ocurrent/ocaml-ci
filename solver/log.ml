let src = Logs.Src.create "ocaml_ci_solver" ~doc:"ocaml-ci dependency solver"
include (val Logs.src_log src : Logs.LOG)

let pp_timestamp f x =
  let open Unix in
  let tm = gmtime x in
  Fmt.pf f "%04d-%02d-%02d %02d:%02d.%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let reporter =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stderr ("%a %14s %a @[" ^^ fmt ^^ "@]@.")
      pp_timestamp (Unix.gettimeofday ())
      src
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }
