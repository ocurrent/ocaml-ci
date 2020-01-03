module type DOCKER_CONTEXT = sig

  type image

  val image_hash : image -> string

  val pull : schedule:Current_cache.Schedule.t -> string -> image Current.t

  val build :
    label:string ->
    dockerfile:Dockerfile.t Current.t ->
    Current_docker.S.source ->
    image Current.t

  val run :
    label:string ->
    image Current.t ->
    args:string list ->
    unit Current.t

end
