open Current.Syntax
open Ocaml_ci

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

(* Maximum time for one Docker build. *)
let timeout = Duration.of_hour 1

let default_compiler = "4.09"

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash = Uri.of_string (Printf.sprintf "https://ci.ocamllabs.io/github/%s/%s/commit/%s" owner name hash)

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

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

let job_id x =
  let+ job = Current.Analysis.get x in
  Current.Analysis.job_id job

module Docker_of_builder (Builder : Conf.BUILDER) : Ocaml_ci.S.DOCKER_CONTEXT = struct

  type image = Builder.Image.t

  let image_hash = Builder.Image.hash

  let pull name =
    Builder.pull ~schedule:weekly name

  let build ~label ~dockerfile source =
    Builder.build ~label ~pool:Builder.pool ~pull:false ~dockerfile source

  let run ~label image ~args =
    Builder.run ~label ~pool:Builder.pool image ~args

end

module Lint = Ocaml_ci.Lint.Make (Docker_of_builder (Conf.Builder_amd1))

let build_with_docker ~repo ~analysis src =
  let info =
    let+ info = analysis in
    let opam_files = Analyse.Analysis.opam_files info in
    if opam_files = [] then failwith "No opam files found!";
    info
  in
  let build (module Docker : Conf.BUILDER) variant =
    let dockerfile =
      let+ base = Docker.pull ~schedule:weekly ("ocurrent/opam:" ^ variant)
      and+ repo = repo
      and+ info = info in
      Opam_build.dockerfile ~base:(Docker.Image.hash base) ~info ~repo ~variant
    in
    let build = Docker.build ~timeout ~pool:Docker.pool ~pull:false ~dockerfile (`Git src) in
    let result = Current.map (fun _ -> `Built) build in
    result, job_id build
  in
  let lint_result = Lint.v ~analysis ~src in
  [
    (* Compiler versions:*)
    "4.10", build (module Conf.Builder_amd1) "debian-10-ocaml-4.10";
    "4.09", build (module Conf.Builder_amd3) "debian-10-ocaml-4.09";
    "4.08", build (module Conf.Builder_amd1) "debian-10-ocaml-4.08";
    "4.07", build (module Conf.Builder_amd2) "debian-10-ocaml-4.07";
    "4.06", build (module Conf.Builder_amd2) "debian-10-ocaml-4.06";
    "4.05", build (module Conf.Builder_amd3) "debian-10-ocaml-4.05";
    "4.04", build (module Conf.Builder_amd3) "debian-10-ocaml-4.04";
    "4.03", build (module Conf.Builder_amd2) "debian-10-ocaml-4.03";
    "4.02", build (module Conf.Builder_amd2) "debian-10-ocaml-4.02";
    (* Distributions: *)
    "alpine", build (module Conf.Builder_amd1) @@ "alpine-3.10-ocaml-" ^ default_compiler;
    "ubuntu", build (module Conf.Builder_amd2) @@ "ubuntu-19.04-ocaml-" ^ default_compiler;
    "opensuse", build (module Conf.Builder_amd2) @@ "opensuse-15.1-ocaml-" ^ default_compiler;
    "centos", build (module Conf.Builder_amd3) @@ "centos-7-ocaml-" ^ default_compiler;
    "fedora", build (module Conf.Builder_amd3) @@ "fedora-30-ocaml-" ^ default_compiler;
    (* oraclelinux doesn't work in opam 2 yet: *)
    (* build (module Conf.Builder_amd3) @@ "oraclelinux-7-ocaml-" ^ default_compiler; *)
    "lint", (lint_result, job_id lint_result);
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
      | [] -> assert false
      | [ msg, _ ] when ok = 0 -> msg (* Everything failed with the same error *)
      | [ msg, ls ] -> Fmt.strf "%a failed: %s" Fmt.(list ~sep:(unit ", ") string) ls msg
      | _ ->
        (* Multiple error messages; just list everything that failed. *)
        let pp_label f (_, l) = Fmt.string f l in
        Fmt.strf "%a failed" Fmt.(list ~sep:(unit ", ") pp_label) errs
    ))

let summarise results =
  results
  |> List.map (fun (label, build) ->
      let+ result = Current.state build ~hidden:true in
      (label, result)
    )
  |> Current.list_seq
  |> Current.map @@ fun results ->
  results |> List.fold_left (fun (ok, pending, err, skip) -> function
      | _, Ok (`Checked | `Check_skipped) -> (ok, pending, err, skip)  (* Don't count lint checks *)
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
  and analysis = Analyse.examine src in
  Current.component "summarise" |>
  let** result =
    build_with_docker ~repo ~analysis src
    |> List.map (fun (variant, (build, _job)) -> variant, build)
    |> summarise
  in
  Current.of_output result

let v ~app () =
  Github.App.installations app |> Current.list_iter ~pp:Github.Installation.pp @@ fun installation ->
  let repos = Github.Installation.repositories installation in
  repos |> Current.list_iter ~pp:Github.Api.Repo.pp @@ fun repo ->
  let refs = Github.Api.Repo.ci_refs repo |> set_active_refs ~repo in
  refs |> Current.list_iter ~pp:Github.Api.Commit.pp @@ fun head ->
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  let analysis = Analyse.examine src in
  let builds =
    let repo = Current.map Github.Api.Repo.id repo in
    build_with_docker ~repo ~analysis src in
  let jobs = builds
             |> List.map (fun (variant, (_build, job)) ->
                 let+ x = job in
                 (variant, x)
               )
             |> Current.list_seq
  in
  let index =
    let+ commit = head
    and+ analysis = job_id analysis
    and+ jobs = jobs in
    let repo = Current_github.Api.Commit.repo_id commit in
    let hash = Current_github.Api.Commit.hash commit in
    Index.record ~repo ~hash @@ ("(analysis)", analysis) :: jobs
  in
  let set_status =
    builds
    |> List.map (fun (variant, (build, _job)) -> variant, build)
    |> summarise
    |> github_status_of_state ~head
    |> Github.Api.Commit.set_status head "ocaml-ci"
  in
  Current.all [index; set_status]
