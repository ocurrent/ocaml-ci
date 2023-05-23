module Build = Representation.Build
module Step = Representation.Step
module Client = Ocaml_ci_api.Client
module Run_time = Ocaml_ci.Run_time

let test_to_json (jobs, build_status, build_created_at, expected) =
  let result =
    Build.to_json
    @@ Build.from_jobs_status ~jobs ~build_status ~build_created_at
         ~step_route_prefix:"/github/foo/bar"
  in
  Alcotest.(check string) "to_json" expected result

(* NOTE that for the purposes of testing, the timezone has been set to UTC
   by adding timedesc-tzlocal.utc to the dune file. See the README for timedesc *)
let test_simple () =
  let step_info_1 =
    Client.create_job_info "(analysis)" Passed ~queued_at:(Some 1666210392.)
      ~started_at:(Some 1666210434.) ~finished_at:(Some 1666210490.)
  in
  let expected_1 =
    {|{"version":"1.0","status":"passed","created_at":"Oct 19 20:13 +00:00","finished_at":"Oct 19 20:14 +00:00","queued_for":"42s","ran_for":"56s","can_rebuild":false,"can_cancel":false,"variant":"(analysis)","is_experimental":false}|}
  in
  let step_info_2 =
    Client.create_job_info "variant" Passed ~queued_at:(Some 1666210392.)
      ~started_at:(Some 1666210434.) ~finished_at:(Some 1666210500.)
  in
  let expected_2 =
    {|{"version":"1.0","status":"passed","created_at":"Oct 19 20:13 +00:00","finished_at":"Oct 19 20:15 +00:00","queued_for":"42s","ran_for":"1m06s","can_rebuild":false,"can_cancel":false,"variant":"variant","is_experimental":false}|}
  in
  let step_info_3 =
    Client.create_job_info "variant" (Failed "For reasons")
      ~queued_at:(Some 1666210392.) ~started_at:(Some 1666210434.)
      ~finished_at:(Some 1666210550.)
  in
  let expected_3 =
    {|{"version":"1.0","status":"failed: For reasons","created_at":"Oct 19 20:13 +00:00","finished_at":"Oct 19 20:15 +00:00","queued_for":"42s","ran_for":"1m56s","can_rebuild":false,"can_cancel":false,"variant":"variant","is_experimental":false}|}
  in
  let jobs = [ step_info_1; step_info_2; step_info_3 ] in
  let build_status : Client.State.t = Failed "for reasons" in
  let build_created_at = 1666210300. in
  let expected =
    Fmt.str "%s[%s,%s,%s],\"step_route_prefix\":\"/github/foo/bar\"}"
      {|{"version":"1.0","status":"failed: for reasons","first_created_at":"Oct 19 20:13 +00:00","ran_for":"4m16s","total_ran_for":"6m04s","can_cancel":false,"can_rebuild":true,"steps":|}
      expected_1 expected_2 expected_3
  in
  List.iter test_to_json [ (jobs, build_status, build_created_at, expected) ]

let tests = [ Alcotest_lwt.test_case_sync "simple" `Quick test_simple ]
