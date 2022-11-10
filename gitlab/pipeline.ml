open Current.Syntax
open Ocaml_ci
module Git = Current_git
module Gitlab = Current_gitlab
module Docker = Current_docker.Default

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "gitlab"

  let repositories_total =
    let help = "Total number of active repositories" in
    Gauge.v_label ~label_name:"account" ~help ~namespace ~subsystem
      "repositories_total"
end

let platforms =
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 30) () in
  let v
      {
        Ocaml_ci_service.Conf.label;
        builder;
        pool;
        distro;
        ocaml_version;
        arch;
        opam_version;
      } =
    let base =
      Platform.pull ~arch ~schedule ~builder ~distro ~ocaml_version
        ~opam_version
    in
    let host_base =
      match arch with
      | `X86_64 -> base
      | _ ->
          Platform.pull ~arch:`X86_64 ~schedule ~builder ~distro ~ocaml_version
            ~opam_version
    in
    Platform.get ~arch ~label ~builder ~pool ~distro ~ocaml_version ~host_base
      ~opam_version base
  in
  let v2_1 = Ocaml_ci_service.Conf.platforms `V2_1 in
  Current.list_seq (List.map v v2_1)

let program_name = "ocaml-ci"

(* Link for GitLab statuses. *)
let url ~owner ~name ~hash =
  Uri.of_string
    (Printf.sprintf "https://ci.ocamllabs.io/gitlab/%s/%s/commit/%s" owner name
       hash)

let opam_repository_commit =
  let module Github = Current_github in
  let repo = { Github.Repo_id.owner = "ocaml"; name = "opam-repository" } in
  Github.Api.Anonymous.head_of repo @@ `Ref "refs/heads/master"

(* TODO Sometime later, grab these from Index/DB table. *)
let gitlab_repos : Gitlab.Repo_id.t list =
  [
    {
      Gitlab.Repo_id.owner = "tmcgilchrist";
      Gitlab.Repo_id.name = "ocaml-gitlab";
      project_id = 29798678;
    };
    {
      Gitlab.Repo_id.owner = "talex5";
      Gitlab.Repo_id.name = "gemini-eio";
      project_id = 28169706;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "resto";
      project_id = 16269987;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "data-encoding";
      project_id = 14134943;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "json-data-encoding";
      project_id = 16489740;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "ringo";
      project_id = 15200071;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "ocaml-secp256k1-internal";
      project_id = 17524462;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "lwt-exit";
      project_id = 22943026;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "lwt-watcher";
      project_id = 14672501;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "lwt-canceler";
      project_id = 14702762;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "ctypes_stubs_js";
      project_id = 31956063;
    };
  ]

(* Fake Installation module, we don't have this in GitLab. *)
module Installation = struct
  type t = { name : string }

  let compare = compare
  let pp f t = Fmt.string f t.name
end

let gitlab_installations =
  gitlab_repos
  |> List.map (fun x -> { Installation.name = x.Gitlab.Repo_id.owner })
  |> List.sort_uniq Installation.compare

let set_active_installations (accounts : Installation.t list Current.t) =
  let+ accounts = accounts in
  accounts
  |> List.fold_left
       (fun acc i -> Index.Owner_set.add i.Installation.name acc)
       Index.Owner_set.empty
  |> Index.set_active_owners;
  accounts

let set_active_repos ~(installation : Installation.t Current.t)
    (repos : Gitlab.Repo_id.t list Current.t) =
  let+ repos = repos and+ installation = installation in
  repos
  |> List.fold_left
       (fun acc r -> Index.Repo_set.add r.Gitlab.Repo_id.name acc)
       Index.Repo_set.empty
  |> Index.set_active_repos ~owner:installation.Installation.name;
  Prometheus.Gauge.set
    (Metrics.repositories_total installation.Installation.name)
    (float_of_int (List.length repos));
  repos

let set_active_refs ~(repo : Gitlab.Repo_id.t Current.t) xs =
  let+ repo = repo and+ xs = xs in
  let repo' = { Repo_id.owner = repo.owner; name = repo.name } in
  Index.set_active_refs ~repo:repo'
    (xs
    |> List.fold_left
         (fun acc x ->
           let commit = Gitlab.Api.Commit.id x in
           let gref = Git.Commit_id.gref commit in
           let hash = Git.Commit_id.hash commit in
           (* FIXME [benmandrew]: Implement fields for Gitlab *)
           Index.Ref_map.add gref { Index.hash; message = ""; name = "" } acc)
         Index.Ref_map.empty);
  xs

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with Some { Current.Metadata.job_id; _ } -> job_id | None -> None

let build_with_docker ?ocluster ~repo ~analysis source =
  let repo' =
    Current.map
      (fun r ->
        { Repo_id.owner = r.Gitlab.Repo_id.owner; Repo_id.name = r.name })
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
                 Cluster_build.v ocluster ~platforms ~repo:repo' ~spec src
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

let repositories (installation : Installation.t Current.t) :
    Gitlab.Repo_id.t list Current.t =
  let+ installation = installation in
  List.filter
    (fun repo -> repo.Gitlab.Repo_id.owner == installation.Installation.name)
    gitlab_repos

let gitlab_status_of_state head result =
  let+ head = head and+ result = result in
  let { Gitlab.Repo_id.owner; name; project_id = _ } =
    Gitlab.Api.Commit.repo_id head
  in
  let hash = Gitlab.Api.Commit.hash head in
  let url = url ~owner ~name ~hash in
  match result with
  | Ok _ ->
      Gitlab.Api.Status.v ~url `Success ~description:"Passed" ~name:program_name
  | Error (`Active _) -> Gitlab.Api.Status.v ~url `Pending ~name:program_name
  | Error (`Msg m) ->
      Gitlab.Api.Status.v ~url `Failure ~description:m ~name:program_name

let v ?ocluster ~app ~solver () =
  let ocluster =
    Option.map (Cluster_build.config ~timeout:(Duration.of_hour 1)) ocluster
  in
  Current.with_context opam_repository_commit @@ fun () ->
  Current.with_context platforms @@ fun () ->
  Current.return gitlab_installations
  |> set_active_installations
  |> Current.list_iter ~collapse_key:"org" (module Installation)
     @@ fun installation ->
     let repos = repositories installation |> set_active_repos ~installation in
     repos
     |> Current.list_iter ~collapse_key:"repo" (module Gitlab.Repo_id)
        @@ fun repo ->
        let* repo_id = repo in
        let refs =
          Gitlab.Api.ci_refs app ~staleness:Ocaml_ci_service.Conf.max_staleness
            repo_id
          |> set_active_refs ~repo
        in
        refs
        |> Current.list_iter (module Gitlab.Api.Commit) @@ fun head ->
           let src = Git.fetch (Current.map Gitlab.Api.Commit.id head) in
           let analysis =
             Analyse.examine ~solver ~platforms ~opam_repository_commit src
           in
           let builds = build_with_docker ?ocluster ~repo ~analysis src in
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
             let gref = Git.Commit_id.gref @@ Gitlab.Api.Commit.id commit in
             let repo = Gitlab.Api.Commit.repo_id commit in
             let repo' =
               { Ocaml_ci.Repo_id.owner = repo.owner; name = repo.name }
             in
             let hash = Gitlab.Api.Commit.hash commit in
             let jobs =
               builds
               |> List.map (fun (variant, (_, job_id)) -> (variant, job_id))
             in
             Index.record ~repo:repo' ~hash ~status ~gref jobs
           and set_gitlab_status =
             gitlab_status_of_state head summary
             |> Gitlab.Api.Commit.set_status head "ocaml-ci"
           in
           Current.all [ index; set_gitlab_status ]
