module Make : functor (_ : Git_forge_intf.Forge) -> sig
  val list :
    org:string ->
    repo:string ->
    default_ref:string ->
    refs:Git_forge_intf.Client.Repo.ref_info Git_forge_intf.Client.Ref_map.t ->
    string
end
