(* Access control policy. *)
let has_role user = function
  | `Viewer | `Monitor -> true
  | _ ->
    match Option.map Current_web.User.id user with
    | Some ( "github:talex5"
           | "github:avsm"
           | "github:kit-ty-kate"
           | "github:samoht"
           | "github:tmcgilchrist"
           | "github:dra27"
           ) -> true
    | _ -> false

let webhook_route ~engine ~webhook_secret ~has_role =
  Routes.(s "webhooks" / s "github" /? nil @--> Current_github.webhook ~engine ~webhook_secret ~has_role)

let login_route github_auth = Routes.(s "login" /? nil @--> Current_github.Auth.login github_auth) 

let authn github_auth = Option.map Current_github.Auth.make_login_uri github_auth