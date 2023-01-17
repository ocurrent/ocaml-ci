(** Activate BuildKit experimental syntax. *)

val add : Ocaml_version.arch -> string
(** [add arch] will activate BuildKit experimental syntax with a hash that will
    work for that architecture. Defaults to x86_64 if no arch is specified. *)
