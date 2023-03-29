module Ev = Brr.Ev
module El = Brr.El
module At = Brr.At
module Document = Brr.Document
module Window = Brr.Window

let go first_line_repro_block last_line_repro_block =
  (* extractStepsToReproduce is declared in step-page-poll.js
     Until it is cut across to jsoo/brr call it directly *)
  let extractStepsToReproduce' = Jv.get Jv.global "extractStepsToReproduce" in
  let _ =
    Jv.apply extractStepsToReproduce'
      [| Jv.of_int first_line_repro_block; Jv.of_int last_line_repro_block |]
  in
  ()
