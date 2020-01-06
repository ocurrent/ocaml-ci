module type DOCKER_CONTEXT = sig

  type source
  type image

  val image_hash : image -> string

  val pull : string -> image Current.t

  val build :
    label:string ->
    dockerfile:Dockerfile.t Current.t ->
    source ->
    image Current.t

  val run :
    label:string ->
    image Current.t ->
    args:string list ->
    unit Current.t

end
