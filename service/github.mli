val has_role :
  Current_web.User.t option ->
  [< `Admin | `Builder | `Monitor | `Viewer ] ->
  bool

val webhook_route :
  engine:Current.Engine.t ->
  get_job_ids:(owner:string -> name:string -> hash:string -> string list) ->
  webhook_secret:string ->
  Current_web.Resource.t Routes.route

val login_route :
  Current_github.Auth.t option -> Current_web.Resource.t Routes.route

val authn : Current_github.Auth.t option -> (csrf:string -> Uri.t) option

type t = { account : string; bio : string; avatar_url : string }
type owner_t = User | Org

val get_repo_owner :
  api:Current_github.Api.t -> account:string -> owner_t -> t Lwt.t
