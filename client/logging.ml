let reporter =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stdout
      ("%a %a @[" ^^ fmt ^^ "@]@.")
      Fmt.(styled `Magenta string)
      (Printf.sprintf "%14s" src) Logs_fmt.pp_header (level, header)
  in
  { Logs.report }

let init ?(level = Logs.Info) () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some level);
  Logs.set_reporter reporter
