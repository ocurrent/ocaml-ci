module Step = Representation.Step
module Client = Ocaml_ci_api.Client
module Run_time = Ocaml_ci_client_lib.Run_time

let test_to_json (step_info, run_time, expected) =
  let result =
    Step.to_json @@ Step.from_status_info_run_time ~step_info ~run_time
  in
  Alcotest.(check string) "to_json" expected result

(* NOTE that for the purposes of testing, the timezone has been set to UTC
   by adding timedesc-tzlocal.utc to the dune file. See the README for timedesc *)
let test_simple () =
  let step_info_1 : Client.job_info option =
    Some
      {
        variant = "variant";
        outcome = Active;
        queued_at = Some 1666210392.;
        started_at = Some 1666210434.;
        finished_at = None;
      }
  in
  let run_time_1 : Run_time.run_time_info option =
    Some (Running { queued_for = 42.2; ran_for = 0. })
  in
  let expected_1 =
    {|{"version":"1.0","status":"active","created_at":"Oct 19 20:13 +00:00","finished_at":"-","queued_for":"42s","ran_for":"0s"}|}
  in
  let step_info_2 : Client.job_info option =
    Some
      {
        variant = "variant";
        outcome = Passed;
        queued_at = Some 1666210392.;
        started_at = Some 1666210434.;
        finished_at = Some 1666210500.;
      }
  in
  let run_time_2 : Run_time.run_time_info option =
    Some (Finished { queued_for = 42.2; ran_for = Some 5.4 })
  in
  let expected_2 =
    {|{"version":"1.0","status":"passed","created_at":"Oct 19 20:13 +00:00","finished_at":"Oct 19 20:15 +00:00","queued_for":"42s","ran_for":"5s"}|}
  in
  let step_info_3 : Client.job_info option =
    Some
      {
        variant = "variant";
        outcome = Failed "For reasons";
        queued_at = Some 1666210392.;
        started_at = Some 1666210434.;
        finished_at = None;
      }
  in
  let run_time_3 : Run_time.run_time_info option =
    Some (Finished { queued_for = 42.2; ran_for = Some 5.4 })
  in
  let expected_3 =
    {|{"version":"1.0","status":"failed: For reasons","created_at":"Oct 19 20:13 +00:00","finished_at":"-","queued_for":"42s","ran_for":"5s"}|}
  in

  List.iter test_to_json
    [
      (step_info_1, run_time_1, expected_1);
      (step_info_2, run_time_2, expected_2);
      (step_info_3, run_time_3, expected_3);
    ]

let tests = [ Alcotest_lwt.test_case_sync "simple" `Quick test_simple ]
