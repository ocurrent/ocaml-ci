(* Access control policy. *)
let has_role user = function
  | `Viewer | `Monitor -> true
  | `Builder | `Admin -> (
    match Option.map Current_web.User.id user with
    | Some "gitlab:tmcgilchrist" -> true
    | Some _ | None -> false)

let webhook_route ~webhook_secret =
  Routes.(
    (s "webhooks" / s "gitlab" /? nil)
    @--> Current_gitlab.webhook ~webhook_secret)

let login_route gitlab_auth =
  Routes.((s "login" /? nil) @--> Current_gitlab.Auth.login gitlab_auth)

let authn auth = Option.map Current_gitlab.Auth.make_login_uri auth