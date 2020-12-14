(** Persist the [_build] directory between runs.
    Note that this different from the "dune cache" as described in
    https://dune.readthedocs.io/en/stable/caching.html. *)
val for_repo: Current_github.Repo_id.t -> Obuilder_spec.Cache.t
