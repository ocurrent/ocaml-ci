val vars: Ocaml_version.t -> Ocaml_ci_api.Worker.Vars.t list
(** [vars ov] is a list of operating system variables along with an ocaml version [ov]
    that can be fed to the solver *)