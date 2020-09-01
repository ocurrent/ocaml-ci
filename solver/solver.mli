module Response : sig
  type ('a, 'b) result = ('a, 'b) Stdlib.result =
    | Ok of 'a
    | Error of 'b
  [@@deriving yojson]

  type selection = {
    packages : string list;
    post_packages : string list;
  }
  [@@deriving yojson]

  type t = (selection, [`Solve of string]) result [@@deriving yojson]
end

val main : Git_unix.Store.Hash.t -> unit
(** [main hash] runs a worker process that reads requests from stdin and writes results to stdout,
    using commit [hash] in opam-repository. *)
