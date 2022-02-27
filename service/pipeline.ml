open Current.Syntax
open Ocaml_ci

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

let platforms =
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 30) () in
  let v { Conf.label; builder; pool; distro; ocaml_version; arch; opam_version } =
    let base = Platform.pull ~arch ~schedule ~builder ~distro ~ocaml_version ~opam_version in
    let host_base =
      match arch with
      | `X86_64 -> base
      | _ -> Platform.pull ~arch:`X86_64 ~schedule ~builder ~distro ~ocaml_version ~opam_version
    in
    Platform.get ~arch ~label ~builder ~pool ~distro ~ocaml_version ~host_base ~opam_version base
  in
  let v2_0 = Conf.platforms `V2_0 in
  let v2_1 = Conf.platforms `V2_1 in
  Current.list_seq (List.map v (v2_0 @ v2_1))

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash = Uri.of_string (Printf.sprintf "https://ci.ocamllabs.io/github/%s/%s/commit/%s" owner name hash)

(* Link for GitHub CheckRun details. *)
let url_variant ~owner ~name ~hash ~variant =
  Printf.sprintf "https://ci.ocamllabs.io/github/%s/%s/commit/%s/variant/%s" owner name hash variant

let opam_repository_commit =
  let repo = { Github.Repo_id.owner = "ocaml"; name = "opam-repository" } in
  Github.Api.Anonymous.head_of repo @@ `Ref "refs/heads/master"

let github_status_of_state ~head result results =
  let+ head = head
  and+ result = result
  and+ results = results in
  let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
  let hash = Github.Api.Commit.hash head in
  let url = url ~owner ~name ~hash in
  let pp_status f = function (variant, (build, _job_id)) ->
    let job_url = url_variant ~owner ~name ~hash ~variant in
    match build with
      | Ok `Checked | Ok `Built->
        Fmt.pf f "%s [%s (%s)](%s)" "✅" variant "passed" job_url
      | Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m ->
        Fmt.pf f "%s [%s (%s)](%s)" "¯\\_(ツ)_/¯" variant "skipped" job_url
      | Error `Msg m ->
        Fmt.pf f "%s [%s (%s)](%s)" "❌" variant ("failed: " ^ m) job_url
      | Error `Active _ ->
        Fmt.pf f "%s [%s (%s)](%s)" "🟠" variant "active" job_url in
  let summary = Fmt.str "@[<v>%a@]" (Fmt.list ~sep:Fmt.cut pp_status)
      (List.sort (fun (x, _) (y,_) -> String.compare x y) results) in
  match result with
  | Ok _ ->
     Github.Api.CheckRunStatus.v ~url (`Completed `Success) ~summary
  | Error (`Active _) ->
     Github.Api.CheckRunStatus.v ~url `Queued ~summary
  | Error (`Msg m) when Astring.String.is_prefix ~affix:"[SKIP]" m ->
     Github.Api.CheckRunStatus.v ~url (`Completed (`Skipped m)) ~summary
  | Error (`Msg m) ->
     Github.Api.CheckRunStatus.v ~url (`Completed (`Failure m)) ~summary

module Matrix = struct
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

  let result_symbol =
    function
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

end

let set_active_installations installations =
  let+ installations = installations in
  installations
  |> List.fold_left (fun acc i -> Index.Owner_set.add (Github.Installation.account i) acc) Index.Owner_set.empty
  |> Index.set_active_owners;
  installations

let set_active_repos ~installation repos =
  let+ installation = installation
  and+ repos = repos in
  let owner = Github.Installation.account installation in
  repos
  |> List.fold_left (fun acc r -> Index.Repo_set.add (Github.Api.Repo.id r).name acc) Index.Repo_set.empty
  |> Index.set_active_repos ~owner;
  repos

let set_active_refs ~repo xs =
  let+ repo = repo
  and+ xs = xs in
  let repo = Github.Api.Repo.id repo in
  Index.set_active_refs ~repo (
    xs |> List.fold_left (fun acc x ->
        let commit = Github.Api.Commit.id x in
        let gref = Git.Commit_id.gref commit in
        let hash = Git.Commit_id.hash commit in
        Index.Ref_map.add gref hash acc
      ) Index.Ref_map.empty
  );
  xs

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with
  | Some { Current.Metadata.job_id; _ } -> job_id
  | None -> None

let build_with_docker ?ocluster ~repo ~analysis source =
  Current.with_context analysis @@ fun () ->
  let specs =
    let+ analysis = Current.state ~hidden:true analysis in
    match analysis with
    | Error _ ->
        (* If we don't have the analysis yet, just use the empty list. *)
        []
    | Ok analysis ->
      match Analyse.Analysis.selections analysis with
      | `Opam_monorepo builds ->
        let lint_selection = Opam_monorepo.selection_of_config (List.hd builds) in
        Spec.opam ~label:"(lint-fmt)" ~selection:lint_selection ~analysis (`Lint `Fmt)
        :: List.map (fun config -> Spec.opam_monorepo ~config) builds
      | `Opam_build selections ->
        let lint_selection = List.hd selections in
        let builds =
          selections
          |> Selection.filter_duplicate_opam_versions
          |> List.map (fun selection ->
               let label = Variant.to_string selection.Selection.variant in
               Spec.opam ~label ~selection ~analysis `Build
             )
        and lint =
          [
            Spec.opam ~label:"(lint-fmt)" ~selection:lint_selection ~analysis (`Lint `Fmt);
            Spec.opam ~label:"(lint-doc)" ~selection:lint_selection ~analysis (`Lint `Doc);
            Spec.opam ~label:"(lint-opam)" ~selection:lint_selection ~analysis (`Lint `Opam);
          ]
        in
        lint @ builds
  in
  let builds = specs |> Current.list_map (module Spec) (fun spec ->
      let+ result =
        match ocluster with
        | None -> Build.v ~platforms ~repo ~spec source
        | Some ocluster ->
          let src = Current.map Git.Commit.id source in
          Cluster_build.v ocluster ~platforms ~repo ~spec src
      and+ spec = spec in
      Spec.label spec, result
    ) in
  let+ builds = builds
  and+ analysis_result = Current.state ~hidden:true (Current.map (fun _ -> `Checked) analysis)
  and+ analysis_id = get_job_id analysis in
  builds @ [
    "(analysis)", (analysis_result, analysis_id);
  ]

let list_errors ~ok errs =
  let groups =  (* Group by error message *)
    List.sort compare errs |> List.fold_left (fun acc (msg, l) ->
        match acc with
        | (m2, ls) :: acc' when m2 = msg -> (m2, l :: ls) :: acc'
        | _ -> (msg, [l]) :: acc
      ) []
  in
  Error (`Msg (
      match groups with
      | [] -> "No builds at all!"
      | [ msg, _ ] when ok = 0 -> msg (* Everything failed with the same error *)
      | [ msg, ls ] -> Fmt.str "%a failed: %s" Fmt.(list ~sep:(any ", ") string) ls msg
      | _ ->
        (* Multiple error messages; just list everything that failed. *)
        let pp_label f (_, l) = Fmt.string f l in
        Fmt.str "%a failed" Fmt.(list ~sep:(any ", ") pp_label) errs
    ))

let summarise results =
  results |> List.fold_left (fun (ok, pending, err, skip) -> function
      | _, Ok `Checked -> (ok, pending, err, skip)  (* Don't count lint checks *)
      | _, Ok `Built -> (ok + 1, pending, err, skip)
      | l, Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m -> (ok, pending, err, (m, l) :: skip)
      | l, Error `Msg m -> (ok, pending, (m, l) :: err, skip)
      | _, Error `Active _ -> (ok, pending + 1, err, skip)
    ) (0, 0, [], [])
  |> fun (ok, pending, err, skip) ->
  if pending > 0 then Error (`Active `Running)
  else match ok, err, skip with
    | 0, [], skip -> list_errors ~ok:0 skip (* Everything was skipped - treat skips as errors *)
    | _, [], _ -> Ok ()                     (* No errors and at least one success *)
    | ok, err, _ -> list_errors ~ok err     (* Some errors found - report *)

let local_test ~solver repo () =
  let src = Git.Local.head_commit repo in
  let repo = Current.return { Github.Repo_id.owner = "local"; name = "test" }
  and analysis = Analyse.examine ~solver ~platforms ~opam_repository_commit src in
  Current.component "summarise" |>
  let> results = build_with_docker ~repo ~analysis src in
  let result =
    results
    |> List.map (fun (variant, (build, _job)) -> variant, build)
    |> summarise
  in
  Current_incr.const (result, None)

let v ?ocluster ?matrix ~app ~solver () =
  let ocluster = Option.map (Cluster_build.config ~timeout:(Duration.of_hour 1)) ocluster in
  Current.with_context opam_repository_commit @@ fun () ->
  Current.with_context platforms @@ fun () ->
  let installations = Github.App.installations app |> set_active_installations in
  installations |> Current.list_iter ~collapse_key:"org" (module Github.Installation) @@ fun installation ->
  let repos = Github.Installation.repositories installation |> set_active_repos ~installation in
  let matrix_org_room = Matrix.get_org_room ~installation matrix in
  repos |> Current.list_iter ~collapse_key:"repo" (module Github.Api.Repo) @@ fun repo ->
  let refs = Github.Api.Repo.ci_refs ~staleness:Conf.max_staleness repo |> set_active_refs ~repo in
  let matrix_room = Matrix.get_room ~repo matrix in
  refs |> Current.list_iter (module Github.Api.Commit) @@ fun head ->
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  let analysis = Analyse.examine ~solver ~platforms ~opam_repository_commit src in
  let builds =
    let repo = Current.map Github.Api.Repo.id repo in
    build_with_docker ?ocluster ~repo ~analysis src in
  let summary =
    builds
    |> Current.map (List.map (fun (variant, (build, _job)) -> variant, build))
    |> Current.map summarise
  in
  let status =
    let+ summary = summary in
    match summary with
    | Ok () -> `Passed
    | Error (`Active `Running) -> `Pending
    | Error (`Msg _) -> `Failed
  in
  let index =
    let+ commit = head
    and+ builds = builds
    and+ status = status in
    let repo = Current_github.Api.Commit.repo_id commit in
    let hash = Current_github.Api.Commit.hash commit in
    let jobs = builds |> List.map (fun (variant, (_, job_id)) -> (variant, job_id)) in
    Index.record ~repo ~hash ~status jobs
  and set_github_status =
    builds
    |> github_status_of_state ~head summary
    |> Github.Api.CheckRun.set_status head "ocaml-ci"
  and set_matrix_status =
    match matrix, matrix_room, matrix_org_room with
    | None, None, None -> Current.return ()
    | Some context, Some room, Some org_room ->
      let key =
        let+ head = head in
        Github.Api.Commit.id head
        |> Git.Commit_id.digest
      in
      let message = Matrix.message_of_state ~head summary builds
      in
      Current.all [
        Matrix_current.post context ~key ~room message;
        Matrix_current.post context ~key ~room:org_room message;
      ]
    | _ -> assert false
  in
  Current.all [index; set_github_status; set_matrix_status]
