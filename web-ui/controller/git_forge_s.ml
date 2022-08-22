module type View = View.Git_forge_s.View

module Client = Ocaml_ci_api.Client
module type Controller = sig
  val list_orgs : Client.CI.t -> Dream.response Lwt.t

  val list_repos : org:string -> Client.CI.t -> Dream.server Dream.message Lwt.t

  val list_refs : org:string -> repo:string -> Client.CI.t -> Dream.server Dream.message Lwt.t

  val list_steps : org:string ->
                   repo:string ->
                   hash:string ->
                   Dream.request -> Client.CI.t -> Dream.response Lwt.t

  val show_step : org:string ->
                  repo:string ->
                  hash:string ->
                  variant:string ->
                  Dream.client Dream.message ->
                  Client.CI.t ->
                  Dream.server Dream.message Lwt.t

  val rebuild_step : org:string ->
                     repo:string ->
                     hash:string ->
                     variant:string ->
                     Dream.client Dream.message ->
                     Client.CI.t ->
                     Dream.server Dream.message Lwt.t

  val cancel_steps : org:string ->
                     repo:string ->
                     hash:string ->
                     Dream.request -> Client.CI.t -> Dream.response Lwt.t

  val rebuild_steps : rebuild_failed_only:bool ->
                      org:string ->
                      repo:string ->
                      hash:string ->
                      Dream.client Dream.message ->
                      Client.CI.t ->
                      Dream.server Dream.message Lwt.t
end

