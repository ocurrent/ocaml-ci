module Client = Ocaml_ci_api.Client

type t = Client.Build_status.t

val pp : t Fmt.t
val class_name : t -> string


