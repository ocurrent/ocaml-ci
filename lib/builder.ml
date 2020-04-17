type t = {
  docker_context : string option;
  pool : Current.Pool.t;
  build_timeout : Duration.t;
}

let build { docker_context; pool; build_timeout } ~dockerfile source =
  Current_docker.Raw.build (`Git source)
    ~enable_submodules:false
    ~dockerfile
    ~docker_context
    ~pool
    ~timeout:build_timeout
    ~pull:false

let pull { docker_context; pool = _; build_timeout = _ } tag =
  Current_docker.Raw.pull tag
    ~docker_context

let run { docker_context; pool; build_timeout = _ } ~args img =
  Current_docker.Raw.run img
    ~docker_context
    ~pool
    ~args

let pread { docker_context; pool; build_timeout = _ } ~args img =
  Current_docker.Raw.pread img
    ~docker_context
    ~pool
    ~args
