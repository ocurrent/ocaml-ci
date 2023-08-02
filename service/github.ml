(* Access control policy. *)
let has_role user = function
  | `Viewer | `Monitor -> true
  | `Builder | `Admin -> (
      match Option.map Current_web.User.id user with
      | Some
          ( "github:talex5" | "github:avsm" | "github:kit-ty-kate"
          | "github:mtelvers" | "github:samoht" | "github:tmcgilchrist"
          | "github:dra27" | "github:benmandrew" ) ->
          true
      | Some _ | None -> false)

let webhook_route ~engine ~get_job_ids ~webhook_secret =
  Routes.(
    (s "webhooks" / s "github" /? nil)
    @--> Current_github.webhook ~engine ~get_job_ids ~webhook_secret)

let login_route github_auth =
  Routes.((s "login" /? nil) @--> Current_github.Auth.login github_auth)

let authn github_auth =
  Option.map Current_github.Auth.make_login_uri github_auth
