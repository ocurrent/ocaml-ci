module Step = Representation.Step
module Client = Ocaml_ci_api.Client
module Run_time = Ocaml_ci.Run_time

let test_to_json (step_info, run_time, can_rebuild, can_cancel, expected) =
  let result =
    Step.to_json
    @@ Step.from_status_info_run_time ~step_info ~run_time ~can_rebuild
         ~can_cancel
  in
  Alcotest.(check string) "to_json" expected result

(* NOTE that for the purposes of testing, the timezone has been set to UTC
   by adding timedesc-tzlocal.utc to the dune file. See the README for timedesc *)
let test_simple () =
  let can_rebuild = true in
  let can_cancel = false in
  let step_info_1 =
    Option.some
    @@ Client.create_job_info "variant" Active ~queued_at:(Some 1666210392.)
         ~started_at:(Some 1666210434.) ~finished_at:None
  in
  let run_time_1 : Run_time.TimeInfo.t option =
    Some (Running { queued_for = 42.2; ran_for = 0. })
  in
  let expected_1 =
    {|{"version":"1.0","status":"active","created_at":"Oct 19 20:13 +00:00","finished_at":"-","queued_for":"42s","ran_for":"0s","can_rebuild":true,"can_cancel":false,"variant":"variant","is_experimental":false}|}
  in
  let step_info_2 =
    Option.some
    @@ Client.create_job_info "variant" Passed ~queued_at:(Some 1666210392.)
         ~started_at:(Some 1666210434.) ~finished_at:(Some 1666210500.)
  in
  let run_time_2 : Run_time.TimeInfo.t option =
    Some (Finished { queued_for = 42.2; ran_for = Some 5.4 })
  in
  let expected_2 =
    {|{"version":"1.0","status":"passed","created_at":"Oct 19 20:13 +00:00","finished_at":"Oct 19 20:15 +00:00","queued_for":"42s","ran_for":"5s","can_rebuild":true,"can_cancel":false,"variant":"variant","is_experimental":false}|}
  in
  let step_info_3 =
    Option.some
    @@ Client.create_job_info "variant" (Failed "For reasons")
         ~queued_at:(Some 1666210392.) ~started_at:(Some 1666210434.)
         ~finished_at:None
  in
  let run_time_3 : Run_time.TimeInfo.t option =
    Some (Finished { queued_for = 42.2; ran_for = Some 5.4 })
  in
  let expected_3 =
    {|{"version":"1.0","status":"failed: For reasons","created_at":"Oct 19 20:13 +00:00","finished_at":"-","queued_for":"42s","ran_for":"5s","can_rebuild":true,"can_cancel":false,"variant":"variant","is_experimental":false}|}
  in
  let step_info_4 =
    Option.some
    @@ Client.create_job_info "variant" (Failed "For reasons")
         ~queued_at:(Some 1666210392.) ~started_at:(Some 1666210434.)
         ~finished_at:None ~is_experimental:true
  in
  let run_time_4 : Run_time.TimeInfo.t option =
    Some (Finished { queued_for = 42.2; ran_for = Some 5.4 })
  in
  let expected_4 =
    {|{"version":"1.0","status":"failed: For reasons","created_at":"Oct 19 20:13 +00:00","finished_at":"-","queued_for":"42s","ran_for":"5s","can_rebuild":true,"can_cancel":false,"variant":"variant","is_experimental":true}|}
  in

  List.iter test_to_json
    [
      (step_info_1, run_time_1, can_rebuild, can_cancel, expected_1);
      (step_info_2, run_time_2, can_rebuild, can_cancel, expected_2);
      (step_info_3, run_time_3, can_rebuild, can_cancel, expected_3);
      (step_info_4, run_time_4, can_rebuild, can_cancel, expected_4);
    ]

let tests = [ Alcotest_lwt.test_case_sync "simple" `Quick test_simple ]
