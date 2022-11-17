open Git_forge

module Make : functor (_ : M_Git_forge) -> sig
  val list : org:string -> repos:Git_forge.Client.Org.repo_info list -> string
end
