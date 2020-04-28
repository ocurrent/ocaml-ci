open Current.Syntax
open Ocaml_ci

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

let platforms =
  let v { Conf.label; builder; distro; ocaml_version } =
    let base = Platform.pull ~schedule:daily ~builder ~distro ~ocaml_version in
    Platform.get ~label ~builder ~distro ~ocaml_version base
  in
  Current.list_seq (List.map v Conf.platforms)

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash = Uri.of_string (Printf.sprintf "https://ci.ocamllabs.io/github/%s/%s/commit/%s" owner name hash)

let github_status_of_state ~head result =
  let+ head = head
  and+ result = result in
  let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
  let hash = Github.Api.Commit.hash head in
  let url = url ~owner ~name ~hash in
  match result with
  | Ok _              -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m)    -> Github.Api.Status.v ~url `Failure ~description:m

let set_active_refs ~repo xs =
  let+ repo = repo
  and+ xs = xs in
  let repo = Github.Api.Repo.id repo in
  Index.set_active_refs ~repo (
    xs |> List.map @@ fun x ->
    let commit = Github.Api.Commit.id x in
    let gref = Git.Commit_id.gref commit in
    let hash = Git.Commit_id.hash commit in
    (gref, hash)
  );
  xs

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with
  | Some { Current.Metadata.job_id; _ } -> job_id
  | None -> None

let build_with_docker ~repo ~analysis source =
  Current.with_context analysis @@ fun () ->
  let specs =
    let+ analysis = Current.state ~hidden:true analysis in
    match analysis with
    | Error _ ->
        (* If we don't have the analysis yet, just use the empty list. *)
        []
    | Ok analysis ->
      match Analyse.Analysis.selections analysis with
      | `Duniverse variants ->
        variants
        |> List.rev_map (fun variant ->
            Build.Spec.duniverse ~label:variant ~variant
          )
      | `Opam_build variants ->
        (* Library (non-duniverse) project *)
        let lint_platform = List.hd variants in
        let builds =
          variants |> List.map (fun variant ->
              Build.Spec.opam ~label:variant ~variant ~analysis `Build
            )
        and lint =
          [
            Build.Spec.opam ~label:"(lint-fmt)" ~variant:lint_platform ~analysis (`Lint `Fmt);
            Build.Spec.opam ~label:"(lint-doc)" ~variant:lint_platform ~analysis (`Lint `Doc);
          ]
        in
        lint @ builds
  in
  let builds = specs |> Current.list_map (module Build.Spec) (fun spec ->
      let+ result = Build.v ~platforms ~repo ~spec source
      and+ spec = spec in
      Build.Spec.label spec, result
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
      | [ msg, ls ] -> Fmt.strf "%a failed: %s" Fmt.(list ~sep:(unit ", ") string) ls msg
      | _ ->
        (* Multiple error messages; just list everything that failed. *)
        let pp_label f (_, l) = Fmt.string f l in
        Fmt.strf "%a failed" Fmt.(list ~sep:(unit ", ") pp_label) errs
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

let local_test repo () =
  let src = Git.Local.head_commit repo in
  let repo = Current.return { Github.Repo_id.owner = "local"; name = "test" }
  and analysis = Analyse.examine ~platforms src in
  Current.component "summarise" |>
  let> results = build_with_docker ~repo ~analysis src in
  let result =
    results
    |> List.map (fun (variant, (build, _job)) -> variant, build)
    |> summarise
  in
  Current_incr.const (result, None)

let v ~app () =
  Current.with_context platforms @@ fun () ->
  Github.App.installations app |> Current.list_iter ~collapse_key:"org" (module Github.Installation) @@ fun installation ->
  let repos = Github.Installation.repositories installation in
  repos |> Current.list_iter ~collapse_key:"repo" (module Github.Api.Repo) @@ fun repo ->
  let refs = Github.Api.Repo.ci_refs repo |> set_active_refs ~repo in
  refs |> Current.list_iter (module Github.Api.Commit) @@ fun head ->
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  let analysis = Analyse.examine ~platforms src in
  let builds =
    let repo = Current.map Github.Api.Repo.id repo in
    build_with_docker ~repo ~analysis src in
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
    summary
    |> github_status_of_state ~head
    |> Github.Api.Commit.set_status head "ocaml-ci"
  in
  Current.all [index; set_github_status]
