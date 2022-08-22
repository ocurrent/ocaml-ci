module Client = Ocaml_ci_api.Client

include Client.Build_status

let class_name (t : t) =
  match t with
    | NotStarted -> "not-started"
    | Failed -> "failed"
    | Passed -> "passed"
    | Pending -> "active"
    | Undefined _ -> "undefined"
