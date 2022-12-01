module Make : functor (_ : Git_forge_intf.Forge) -> sig
  val list :
    org:string -> repos:Git_forge_intf.Client.Org.repo_info list -> string
end
