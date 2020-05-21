(** Divide jobs up into distinct epochs. Any number of jobs can run at the same
    time within an epoch, but changing epoch requires first draining the
    existing jobs, finishing the epoch, and then creating the new one.
    The solver uses this to handle updates to opam-repository (each commit is a
    separate epoch). *)

type 'a t

val v : create:(string -> 'a Lwt.t) -> dispose:('a -> unit Lwt.t) -> unit -> 'a t
(** [v ~create ~dispose ()] is an epoch lock that calls [create] to start a new epoch
    and [dispose] to finish one. A new epoch doesn't start until the old one has been
    disposed. *)

val with_epoch : 'a t -> string -> ('a -> 'b Lwt.t) -> 'b Lwt.t
(** [with_epoch t epoch fn] runs [fn v] with the [v] for [epoch].
    If we are already in [epoch], [fn] runs immediately.
    If we are already in another epoch then we wait for all users in the
    previous epoch to finish, then create a new one, then run [fn]. *)
