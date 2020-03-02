module type DOCKER_CONTEXT = sig

  type source
  type image

  val image_hash : image -> string

  val pull : schedule:Current_cache.Schedule.t -> string -> image Current.t

  val build :
    ?label:string ->
    dockerfile:[`Contents of Dockerfile.t | `File of Fpath.t] Current.t ->
    source ->
    image Current.t

  val run :
    ?label:string ->
    image Current.t ->
    args:string list ->
    string Current.t

end
