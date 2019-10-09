open Current.Syntax

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

let pool_size =
  match Conf.profile with
  | `Production -> 20
  | `Dev -> 1

(* Limit number of concurrent builds. *)
let pool = Current.Pool.create ~label:"docker" pool_size

(* Maximum time for one Docker build. *)
let timeout = Duration.of_hour 1

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash = Uri.of_string (Printf.sprintf "https://ci.ocamllabs.io/github/%s/%s/commit/%s" owner name hash)

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let github_status_of_state ~repo ~head result =
  let+ repo = repo
  and+ head = head
  and+ result = result in
  let { Github.Repo_id.owner; name } = repo in
  let hash = Github.Api.Commit.hash head in
  let url = url ~owner ~name ~hash in
  match result with
  | Ok _              -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m)    -> Github.Api.Status.v ~url `Failure ~description:m

let set_active_refs ~repo xs =
  let+ repo = repo
  and+ xs = xs in
  Index.set_active_refs ~repo (
    xs |> List.map @@ fun x ->
    let commit = Github.Api.Commit.id x in
    let gref = Git.Commit_id.gref commit in
    let hash = Git.Commit_id.hash commit in
    (gref, hash)
  );
  xs

let build_with_docker ~repo src =
  let dockerfile =
    let+ base = Docker.pull ~schedule:weekly "ocurrent/opam:alpine-3.10-ocaml-4.08"
    and+ repo = repo
    and+ info = Analyse.examine src in
    Opam_build.dockerfile ~base ~info ~repo
  in
  Docker.build ~timeout ~pool ~pull:false ~dockerfile (`Git src)

let local_test repo () =
  let src = Git.Local.head_commit repo in
  let repo = Current.return { Github.Repo_id.owner = "local"; name = "test" } in
  build_with_docker ~repo src
  |> Current.ignore_value

let v ~app () =
  Github.App.installations app |> Current.list_iter ~pp:Github.Installation.pp @@ fun installation ->
  let github = Current.map Github.Installation.api installation in
  let repos = Github.Installation.repositories installation in
  repos |> Current.list_iter ~pp:Github.Repo_id.pp @@ fun repo ->
  let refs = Github.Api.ci_refs_dyn github repo |> set_active_refs ~repo in
  refs |> Current.list_iter ~pp:Github.Api.Commit.pp @@ fun head ->
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  let build = build_with_docker ~repo src in
  let index =
    let+ commit = head
    and+ job_id = Current.Analysis.get build |> Current.(map Analysis.job_id) in
    Option.iter (Index.record ~commit) job_id
  in
  let set_status =
    build
    |> Current.state
    |> github_status_of_state ~repo ~head
    |> Github.Api.Commit.set_status head "ocaml-ci"
  in
  Current.all [index; set_status]
