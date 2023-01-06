open Current.Syntax
open Ocaml_ci
module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

let platforms = Conf.fetch_platforms ~include_macos:true ()

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash ~gref =
  Uri.of_string
    (Printf.sprintf "https://ci.ocamllabs.io/github/%s/%s/commit/%s/-/%s" owner
       name hash gref)

(* Link for GitHub CheckRun details. *)
let url_variant ~owner ~name ~hash ~variant ~gref =
  Printf.sprintf
    "https://ci.ocamllabs.io/github/%s/%s/commit/%s/variant/%s/-/%s" owner name
    hash variant gref

let opam_repository_commit =
  let repo = { Github.Repo_id.owner = "ocaml"; name = "opam-repository" } in
  Github.Api.Anonymous.head_of repo @@ `Ref "refs/heads/master"

(* Check whether a variant is considered experimental.
   If it is experimental we allow those builds to fail without
   failing the overall build for a commit.
*)
let experimental_variant variant =
  Astring.String.is_prefix ~affix:"macos-homebrew" variant

let pp_fail prefix f m = Fmt.pf f "%s: %s" prefix (Ansi.strip m)

let github_status_of_state ~head result results =
  let+ head = head and+ result = result and+ results = results in
  let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
  let commit_id = Github.Api.Commit.id head in
  let gref = Current_git.Commit_id.gref commit_id in
  let hash = Github.Api.Commit.hash head in
  let url = url ~owner ~name ~hash ~gref in
  let pp_status f = function
    | variant, (build, _job_id) -> (
        let job_url = url_variant ~owner ~name ~hash ~variant ~gref in
        match build with
        | Ok `Checked | Ok `Built ->
            Fmt.pf f "%s [%s (%s)](%s)" "✅" variant "passed" job_url
        | Error (`Msg m) when Astring.String.is_prefix ~affix:"[SKIP]" m ->
            Fmt.pf f "%s [%s (%s)](%s)" "¯\\_(ツ)_/¯" variant "skipped" job_url
        | Error (`Msg m) when experimental_variant variant ->
            Fmt.pf f "%s [EXPERIMENTAL: %s (%a)](%s)" "❌" variant
              (pp_fail "failure") m job_url
        | Error (`Msg m) ->
            Fmt.pf f "%s [%s (%a)](%s)" "❌" variant (pp_fail "failed") m job_url
        | Error (`Active _) ->
            Fmt.pf f "%s [%s (%s)](%s)" "🟠" variant "active" job_url)
  in
  let summary =
    Fmt.str "@[<v>%a@]"
      (Fmt.list ~sep:Fmt.cut pp_status)
      (List.sort (fun (x, _) (y, _) -> String.compare x y) results)
  in
  match result with
  | Ok _ -> Github.Api.CheckRunStatus.v ~url (`Completed `Success) ~summary
  | Error (`Active _) -> Github.Api.CheckRunStatus.v ~url `Queued ~summary
  | Error (`Msg m) when Astring.String.is_prefix ~affix:"[SKIP]" m ->
      Github.Api.CheckRunStatus.v ~url (`Completed (`Skipped m)) ~summary
  | Error (`Msg m) ->
      Github.Api.CheckRunStatus.v ~url (`Completed (`Failure m)) ~summary

let set_active_installations installations =
  let+ installations = installations in
  installations
  |> List.fold_left
       (fun acc i -> Index.Owner_set.add (Github.Installation.account i) acc)
       Index.Owner_set.empty
  |> Index.set_active_owners;
  installations

let set_active_repos ~installation repos =
  let+ installation = installation and+ repos = repos in
  let owner = Github.Installation.account installation in
  repos
  |> List.fold_left
       (fun acc r -> Index.Repo_set.add (Github.Api.Repo.id r).name acc)
       Index.Repo_set.empty
  |> Index.set_active_repos ~owner;
  repos

let ref_from_commit (x : Github.Api.Commit.t) : string =
  Git.Commit_id.gref @@ Github.Api.Commit.id x

let ref_name c =
  match (Github.Api.Commit.pr_name c, Github.Api.Commit.branch_name c) with
  | Some x, None -> x
  | None, Some x -> x
  | _ -> failwith "Commit is neither a branch nor a PR"

let set_active_refs ~repo refs default_ref =
  let+ repo = repo and+ xs = refs and+ default = default_ref in
  let github_repo = Github.Api.Repo.id repo in
  let repo = { Repo_id.owner = github_repo.owner; name = github_repo.name } in
  let refs =
    xs
    |> List.fold_left
         (fun acc x ->
           let gref = ref_from_commit x in
           let commit = Github.Api.Commit.id x in
           let hash = Git.Commit_id.hash commit in
           let name = ref_name x in
           let message = Github.Api.Commit.message x in
           Index.Ref_map.add gref { Index.hash; message; name } acc)
         Index.Ref_map.empty
  in
  Index.set_active_refs ~repo refs (ref_from_commit default);
  xs

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with Some { Current.Metadata.job_id; _ } -> job_id | None -> None

let build_with_docker ?ocluster ?on_cancel ~repo ~analysis ~platforms source =
  let repo' =
    Current.map
      (fun r ->
        { Repo_id.owner = r.Github.Repo_id.owner; Repo_id.name = r.name })
      repo
  in
  Current.with_context analysis @@ fun () ->
  let specs =
    let+ analysis = Current.state ~hidden:true analysis in
    match analysis with
    | Error _ ->
        (* If we don't have the analysis yet, just use the empty list. *)
        []
    | Ok analysis -> (
        match Analyse.Analysis.selections analysis with
        | `Opam_monorepo builds ->
            let lint_selection =
              Opam_monorepo.selection_of_config (List.hd builds)
            in
            Spec.opam ~label:"(lint-fmt)" ~selection:lint_selection ~analysis
              (`Lint `Fmt)
            :: Spec.opam_monorepo builds
        | `Opam_build selections ->
            let lint_selection = List.hd selections in
            let lint_ocamlformat =
              match Analyse.Analysis.ocamlformat_selection analysis with
              | None -> lint_selection
              | Some selection -> selection
            in
            let builds =
              selections
              |> Selection.filter_duplicate_opam_versions
              |> List.map (fun selection ->
                     let label =
                       Variant.to_string selection.Selection.variant
                     in
                     Spec.opam ~label ~selection ~analysis `Build)
            and lint =
              [
                Spec.opam ~label:"(lint-fmt)" ~selection:lint_ocamlformat
                  ~analysis (`Lint `Fmt);
                Spec.opam ~label:"(lint-doc)" ~selection:lint_selection
                  ~analysis (`Lint `Doc);
                Spec.opam ~label:"(lint-opam)" ~selection:lint_selection
                  ~analysis (`Lint `Opam);
              ]
            in
            lint @ builds)
  in
  let builds =
    specs
    |> Current.list_map
         (module Spec)
         (fun spec ->
           let+ result =
             match ocluster with
             | None -> Build.v ~platforms ~repo:repo' ~spec source
             | Some ocluster ->
                 let src = Current.map Git.Commit.id source in
                 Cluster_build.v ocluster ?on_cancel ~platforms ~repo:repo'
                   ~spec src
           and+ spec = spec in
           (Spec.label spec, result))
  in
  let+ builds = builds
  and+ analysis_result =
    Current.state ~hidden:true (Current.map (fun _ -> `Checked) analysis)
  and+ analysis_id = get_job_id analysis in
  builds @ [ ("(analysis)", (analysis_result, analysis_id)) ]

let list_errors ~ok errs =
  let groups =
    (* Group by error message *)
    List.sort compare errs
    |> List.fold_left
         (fun acc (msg, l) ->
           match acc with
           | (m2, ls) :: acc' when m2 = msg -> (m2, l :: ls) :: acc'
           | _ -> (msg, [ l ]) :: acc)
         []
  in
  Error
    (`Msg
      (match groups with
      | [] -> "No builds at all!"
      | [ (msg, _) ] when ok = 0 ->
          msg (* Everything failed with the same error *)
      | [ (msg, ls) ] ->
          Fmt.str "%a failed: %s" Fmt.(list ~sep:(any ", ") string) ls msg
      | _ ->
          (* Multiple error messages; just list everything that failed. *)
          let pp_label f (_, l) = Fmt.string f l in
          Fmt.str "%a failed" Fmt.(list ~sep:(any ", ") pp_label) errs))

let summarise results =
  results
  |> List.fold_left
       (fun (ok, pending, err, skip) -> function
         | _, Ok `Checked ->
             (ok, pending, err, skip) (* Don't count lint checks *)
         | _, Ok `Built -> (ok + 1, pending, err, skip)
         | l, Error (`Msg m) when Astring.String.is_prefix ~affix:"[SKIP]" m ->
             (ok, pending, err, (m, l) :: skip)
         | l, Error (`Msg _) when experimental_variant l ->
             (ok + 1, pending, err, skip)
         (* Don't fail the commit if an experimental build failed. *)
         | l, Error (`Msg m) -> (ok, pending, (m, l) :: err, skip)
         | _, Error (`Active _) -> (ok, pending + 1, err, skip))
       (0, 0, [], [])
  |> fun (ok, pending, err, skip) ->
  if pending > 0 then Error (`Active `Running)
  else
    match (ok, err, skip) with
    | 0, [], skip ->
        list_errors ~ok:0
          skip (* Everything was skipped - treat skips as errors *)
    | _, [], _ -> Ok () (* No errors and at least one success *)
    | ok, err, _ -> list_errors ~ok err (* Some errors found - report *)

let local_test ~solver repo () =
  let platforms = Conf.fetch_platforms ~include_macos:false () in
  let src = Git.Local.head_commit repo in
  let repo = Current.return { Github.Repo_id.owner = "local"; name = "test" }
  and analysis =
    Analyse.examine ~solver ~platforms ~opam_repository_commit src
  in
  Current.component "summarise"
  |> let> results = build_with_docker ~repo ~analysis ~platforms src in
     let result =
       results
       |> List.map (fun (variant, (build, _job)) -> (variant, build))
       |> summarise
     in
     Current_incr.const (result, None)

let v ?ocluster ~app ~solver ~migrations () =
  let ocluster =
    Option.map (Cluster_build.config ~timeout:(Duration.of_hour 1)) ocluster
  in
  let migrations =
    match migrations with
    | Some path -> Index.migrate path
    | None -> Current.return ()
  in
  Current.with_context migrations @@ fun () ->
  Current.with_context opam_repository_commit @@ fun () ->
  Current.with_context platforms @@ fun () ->
  let installations =
    Github.App.installations app |> set_active_installations
  in
  installations
  |> Current.list_iter ~collapse_key:"org" (module Github.Installation)
     @@ fun installation ->
     let repos =
       Github.Installation.repositories installation
       |> set_active_repos ~installation
     in
     repos
     |> Current.list_iter ~collapse_key:"repo" (module Github.Api.Repo)
        @@ fun repo ->
        let default = Github.Api.Repo.head_commit repo in
        let refs =
          let refs =
            Github.Api.Repo.ci_refs ~staleness:Conf.max_staleness repo
          in
          set_active_refs ~repo refs default
        in
        refs
        |> Current.list_iter (module Github.Api.Commit) @@ fun head ->
           let src = Git.fetch (Current.map Github.Api.Commit.id head) in
           let analysis =
             Analyse.examine ~solver ~platforms ~opam_repository_commit src
           in
           let on_cancel =
             match ocluster with None -> None | Some _ -> Some ignore
           in
           let builds =
             let repo = Current.map Github.Api.Repo.id repo in
             build_with_docker ?ocluster ?on_cancel ~repo ~analysis ~platforms
               src
           in
           let summary =
             builds
             |> Current.map
                  (List.map (fun (variant, (build, _job)) -> (variant, build)))
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
             let+ commit = head and+ builds = builds and+ status = status in
             let gref = ref_from_commit commit in
             let repo = Current_github.Api.Commit.repo_id commit in
             let repo =
               { Ocaml_ci.Repo_id.owner = repo.owner; name = repo.name }
             in
             let hash = Current_github.Api.Commit.hash commit in
             let jobs =
               builds
               |> List.map (fun (variant, (_, job_id)) -> (variant, job_id))
             in
             Index.record ~repo ~hash ~status ~gref jobs
           and set_github_status =
             builds
             |> github_status_of_state ~head summary
             |> Github.Api.CheckRun.set_status head "ocaml-ci"
           and set_matrix_status = Current.return () in
           Current.all [ index; set_github_status; set_matrix_status ]
