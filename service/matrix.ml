open Current.Syntax

module Git = Current_git
module Github = Current_github

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash = Uri.of_string (Printf.sprintf "https://ci.ocamllabs.io/github/%s/%s/commit/%s" owner name hash)

(* Link for GitHub CheckRun details. *)
let url_variant ~owner ~name ~hash ~variant =
  Printf.sprintf "https://ci.ocamllabs.io/github/%s/%s/commit/%s/variant/%s" owner name hash variant

  (*
  Structure of a message

  ```
  <h1> ❌ PR <a>#37</a></h1>
  <ul>
    <li> ✅ <b>analysis</b>: <a>passed</a> </li>
    <li> ✅ <b>debian-10-4.12_arm32</b>: <a>passed</a> </li>
    <li> ✅ <b>lint</b>: <a>passed</a> </li>
    <li> ❌ <b>lint-fmt</b>: <a>failed</a> <blockquote>...</blockquote> </li>
  </ul>
  *)

let parse_git_ref ref = match String.split_on_char '/' ref with
  | ["refs"; "heads"; b] -> `Branch b
  | ["refs"; "pull"; p; "head"] -> `PR p
  | _ -> failwith ("Failed to parse git ref: " ^ ref)

let result_symbol = function
  | Ok `Checked | Ok `Built-> "✅"
  | Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m ->
    "¯\\_(ツ)_/¯"
  | Error `Msg _ -> "❌"
  | Error `Active _ -> "🟠"

module Html = struct

  let pp_message_job ~head f (variant, (build, _)) =
    let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
    let hash = Github.Api.Commit.hash head in
    let job_ci_url = url_variant ~owner ~name ~hash ~variant in
    let result =
      match build with
      | Ok `Checked | Ok `Built->
        "passed"
      | Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m ->
        "skipped"
      | Error `Msg m ->
        "failed <blockquote>"^m^"</blockquote>"
      | Error `Active _ ->
        "active"
    in
    Fmt.pf f "%s <b><a href='%s'>%s</a></b>: %s" (result_symbol build) job_ci_url variant result

  let pp_heading f head =
    let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
    let hash = head |> Github.Api.Commit.id |> Git.Commit_id.hash in
    let ci_url = url ~owner ~name ~hash in
    let hash = String.sub hash 0 7 in
    match head |> Github.Api.Commit.id |> Git.Commit_id.gref |> parse_git_ref with
    | `PR p -> Fmt.pf f "PR <a href='https://github.com/%s/%s/pull/%s'>#%s</a>: job <a href='%a'><code><small>%s</small></code></a>" owner name p p Uri.pp ci_url hash
    | `Branch b -> Fmt.pf f "Branch <a href='https://github.com/%s/%s/tree/%s'>%s</a>: job <a href='%a'><code><small>%s</small></code></a>" owner name b b Uri.pp ci_url hash

  let pp_message_full ~head result f results =
    let pp_message_job ~head f = Fmt.pf f "<li>%a</li>" (pp_message_job ~head) in
    Fmt.pf f "<h1>%s %a</h1> <ul>%a</ul>"
      (result_symbol result)
      pp_heading head
      (Fmt.list (pp_message_job ~head)) (List.sort (fun (x, _) (y,_) -> String.compare x y) results)

end

module Plain = struct

  let pp_message_job ~head f (variant, (build, _)) =
    let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
    let hash = Github.Api.Commit.hash head in
    let job_ci_url = url_variant ~owner ~name ~hash ~variant in
    let result =
      match build with
      | Ok `Checked | Ok `Built->
        "passed"
      | Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m ->
        "skipped"
      | Error `Msg m ->
        "failed: "^m
      | Error `Active _ ->
        "active"
    in
    Fmt.pf f "%s %s (%s): %s" (result_symbol build) variant job_ci_url result

  let pp_heading f head =
    let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
    let hash = head |> Github.Api.Commit.id |> Git.Commit_id.hash in
    let ci_url = url ~owner ~name ~hash in
    let hash = String.sub hash 0 7 in
    match head |> Github.Api.Commit.id |> Git.Commit_id.gref |> parse_git_ref with
    | `PR p -> Fmt.pf f "PR #%s (https://github.com/%s/%s/pull/%s): job %s (%a))" p owner name p hash Uri.pp ci_url
    | `Branch b -> Fmt.pf f "Branch %s (https://github.com/%s/%s/tree/%s): job %s (%a))" b owner name b hash Uri.pp ci_url

  let pp_message_full ~head result f results =
    let pp_message_job ~head f = Fmt.pf f "- %a" (pp_message_job ~head) in
    Fmt.pf f "%s %a:\n%a"
      (result_symbol result)
      pp_heading head
      (Fmt.list ~sep:Fmt.(const string "\n") (pp_message_job ~head)) (List.sort (fun (x, _) (y,_) -> String.compare x y) results)

end

(* In order not to spam the channel on each CI update, we send a full message
   only when the final result is known. *)
let message_of_state ~head result results =
  let+ head = head
  and+ result = result
  and+ results = results
  in
  let result = Result.map (fun () -> `Checked) result in
  let open Matrix_common.Events.Event_content.Message in
  let body, formatted_body = match result with
    | Ok _ | Error (`Msg _) ->
      Fmt.to_to_string (Plain.pp_message_full ~head result) results,
      Fmt.to_to_string (Html.pp_message_full ~head result) results
    | _ ->
      Fmt.str "🟠 %a" (Plain.pp_heading) head,
      Fmt.str "<h2>🟠 %a</h2>" (Html.pp_heading) head
  in
  (Text (Text.make ~body ~format:"org.matrix.custom.html" ~formatted_body ()))

(* power levels (https://matrix.org/docs/spec/client_server/latest#m-room-power-levels)
   any user is 0
   bot user is 100 (creator of the room)
   any user in Conf.matrix_admins is 100

   anyone can send any kind of message event
   power level >50 to send state events (including to change power_levels)
*)
let power_level_content_override =
  let admins = ("@ocaml-ci:ocamllabs.io" :: Conf.matrix_admins) |> List.map (fun admin -> (admin, 100)) in
  let users = admins in
  Current.return @@
  Matrix_common.Events.Event_content.Power_levels.make ~users ()

let get_room_for ~alias =
  function
  | None -> None
  | Some context -> Some (Matrix_current.Room.make context ~alias ~power_level_content_override ())

let get_org_room ~installation =
  let alias =
    let+ installation = installation in
    let account = Github.Installation.account installation in
    "ocaml-ci/" ^ account
  in
  get_room_for ~alias

let get_room ~repo =
  let alias =
    let+ repo = repo in
    let id = Github.Api.Repo.id repo in
    (* we rely on the fact that allowed Github repository names is a subset of allowed Matrix channel names. *)
    "ocaml-ci/" ^ id.owner ^ "/" ^ id.name
  in
  get_room_for ~alias
