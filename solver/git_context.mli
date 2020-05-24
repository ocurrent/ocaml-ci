include Opam_0install.S.CONTEXT

val read_packages :
  Git_unix.Store.t ->
  Git_unix.Store.Hash.t ->
  OpamFile.OPAM.t OpamPackage.Version.Map.t OpamPackage.Name.Map.t Lwt.t
(** [read_packages store commit] is an index of the opam files in [store] at [commit]. *)

val create :
  ?test:OpamPackage.Name.Set.t ->
  ?pins:(OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
  constraints:OpamFormula.version_constraint OpamPackage.Name.Map.t ->
  env:(string -> OpamVariable.variable_contents option) ->
  packages:OpamFile.OPAM.t OpamPackage.Version.Map.t OpamPackage.Name.Map.t ->
  t
