type t = Ocaml_ci_api.Client.Build_status.t

val pp : t Fmt.t
val class_name : t -> string