val add : Ocaml_version.arch option -> Dockerfile.t
(** [add arch] will activate BuildKit experimental syntax with
    a hash that will work for that architecture. Defaults to x86_64
    if no arch is specified. *)
