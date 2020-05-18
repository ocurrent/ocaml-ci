val main : self:string list -> string list -> unit
(** [main ~self args] runs the solver with [args].
    Running the command "self new-args" should result in [main] being called
    with [new-args] in the new process. *)
