module type Api = sig
  module Client = Ocaml_ci_api.Client
  module Run_time = Ocaml_ci_client_lib.Run_time

  val show_step :
    step_info:Client.job_info option ->
    run_time:Run_time.run_time_info option ->
    can_rebuild:bool ->
    Dream.response Lwt.t

  val list_steps :
    org:string ->
    repo:string ->
    hash:string ->
    jobs:Client.job_info list ->
    build_status:Client.State.t ->
    Dream.response Lwt.t
end

(* This module is starting out as very thin wrapper on a string. It's
   meant to help stay consistent with the design intent of implementing
   different git-forges via a functorial approach. As we round out the
   Gitlab work this module may grow. If the implementation between the different
   git-forges turns out to be the same, this module will not grow and we may
   choose to back away from this approach. *)
module type M_Git_forge = sig
  val prefix : string
end

module Make (M : M_Git_forge) = struct
  module Build = Representation.Build
  module Step = Representation.Step
  module Run_time = Ocaml_ci_client_lib.Run_time
  module Client = Ocaml_ci_api.Client

  let step_route_prefix ~org ~repo ~hash =
    Fmt.str "/%s/%s/%s/commit/%s/variant" M.prefix org repo hash

  let show_step ~step_info ~run_time ~can_rebuild =
    Dream.json
    @@ Step.to_json
    @@ Step.from_status_info_run_time ~step_info ~run_time ~can_rebuild

  let list_steps ~org ~repo ~hash ~jobs ~build_status =
    let step_route_prefix = step_route_prefix ~org ~repo ~hash in
    let build_created_at =
      Run_time.build_created_at ~build:jobs
      |> Result.to_option
      |> Option.join
      |> Option.value ~default:0.
    in
    Dream.json
    @@ Build.to_json
    @@ Build.from_jobs_status ~jobs ~build_status ~build_created_at
         ~step_route_prefix
end
