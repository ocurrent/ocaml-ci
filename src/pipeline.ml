open Current.Syntax

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

(* Limit number of concurrent builds. *)
let pool = Lwt_pool.create 4 Lwt.return

(* Link for GitHub statuses. *)
let url = Uri.of_string "https://ci.ocamllabs.io:8100/"

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

(* Generate a Dockerfile for building all the opam packages in the build context. *)
let dockerfile ~base =
  let open Dockerfile in
  from (Docker.Image.hash base) @@
  workdir "/src" @@
  add ~src:["*.opam"] ~dst:"/src/" () @@
  run {| find . -name '*.opam' | sed 's/\(.*\)\/\([^\/]*\).opam/opam pin add -yn \2.dev \1/' | sh - |} @@
  run "opam install . --show-actions --deps-only -t | awk '/- install/{print $3}' | xargs opam depext -iy" @@
  copy ~src:["."] ~dst:"/src/" () @@
  run "opam install -tv ."

let github_status_of_state = function
  | Ok _              -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m)    -> Github.Api.Status.v ~url `Failure ~description:m

let v ~app () =
  let dockerfile =
    let+ base = Docker.pull ~schedule:weekly "ocurrent/opam:alpine-3.10-ocaml-4.08" in
    dockerfile ~base
  in
  Github.App.installations app |> Current.list_iter ~pp:Github.Installation.pp @@ fun installation ->
  let github = Current.map Github.Installation.api installation in
  let repos = Github.Installation.repositories installation in
  repos |> Current.list_iter ~pp:Github.Repo_id.pp @@ fun repo ->
  Github.Api.ci_refs_dyn github repo
  |> Current.list_iter ~pp:Github.Api.Commit.pp @@ fun head ->
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  Docker.build ~pool ~pull:false ~dockerfile (`Git src)
  |> Current.state
  |> Current.map github_status_of_state
  |> Github.Api.Commit.set_status head "ocurrent"
