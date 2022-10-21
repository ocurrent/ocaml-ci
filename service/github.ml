(* Access control policy. *)
let has_role user = function
  | `Viewer | `Monitor -> true
  | `Builder | `Admin -> (
      match Option.map Current_web.User.id user with
      | Some
          ( "github:talex5" | "github:avsm" | "github:kit-ty-kate"
          | "github:samoht" | "github:tmcgilchrist" | "github:dra27" ) ->
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

type t = {
  (* iid : int; *)
  account : string;
  description : string;
  avatar_url : string;
}

open Lwt.Infix

let get_user_query account =
  Printf.sprintf
    {|user(login:%s) {
      login
      bio
      avatarUrl
    }|}
    account

let get_org_query account =
  Printf.sprintf
    {|organisation(login:%s) {
      login
      description
      avatarUrl
    }|}
    account

type owner_t = User | Org

let get_repo_owner ~api ~account owner_type =
  let json_description_key = function User -> "bio" | Org -> "description" in
  let query =
    match owner_type with
    | User -> get_user_query account
    | Org -> get_org_query account
  in
  Current_github.Api.exec_graphql api query >>= fun json ->
  Dream.log "%s" (Yojson.Safe.to_string json);
  let description_key = json_description_key owner_type in
  let open Yojson.Safe.Util in
  let description =
    match member description_key json with `Null -> "" | s -> to_string s
  in
  Lwt.return
    {
      (* iid = to_int (member "id" json); *)
      account = to_string (member "login" json);
      description;
      avatar_url = to_string (member "avatar_url" json);
    }
