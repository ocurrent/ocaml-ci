open Git_forge

module Make : functor (_ : M_Git_forge) -> sig
  val list :
    org:string ->
    repo:string ->
    default_ref:string ->
    refs:Git_forge.Client.Repo.ref_info Git_forge.Client.Ref_map.t ->
    string
end
