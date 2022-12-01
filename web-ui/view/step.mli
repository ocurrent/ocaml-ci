module Make : functor (_ : Git_forge_intf.Forge) -> sig
  val list :
    org:string ->
    repo:string ->
    message:string ->
    refs:string list ->
    hash:string ->
    jobs:Git_forge_intf.Client.job_info list ->
    first_step_queued_at:float option ->
    total_run_time:float ->
    build_run_time:float ->
    ?flash_messages:(string * string) list ->
    ?build_status:Git_forge_intf.Client.State.t ->
    csrf_token:string ->
    unit ->
    string

  val show :
    org:string ->
    repo:string ->
    refs:string list ->
    hash:string ->
    jobs:Git_forge_intf.Client.job_info list ->
    variant:string ->
    job:Current_rpc.Job.t ->
    status:Current_rpc.Job.status ->
    csrf_token:string ->
    timestamps:Git_forge_intf.Run_time.timestamps option ->
    build_created_at:float option ->
    ?flash_messages:(string * string) list ->
    string * int64 ->
    Dream.response Lwt.t
end
