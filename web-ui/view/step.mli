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
    variant:string ->
    job:Current_rpc.Job.t ->
    status:Git_forge_intf.Client.State.t ->
    csrf_token:string ->
    timestamps:Git_forge_intf.Run_time.Timestamp.t option ->
    build_created_at:float option ->
    step_created_at:float option ->
    step_finished_at:float option ->
    can_rebuild:bool ->
    can_cancel:bool ->
    ?flash_messages:(string * string) list ->
    string * int64 ->
    string
end
