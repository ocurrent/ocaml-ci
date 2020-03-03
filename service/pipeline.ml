open Current.Syntax
open Ocaml_ci

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default
module Common = Ocaml_ci_api.Common

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash = Uri.of_string (Printf.sprintf "http://147.75.80.95/github/%s/%s/commit/%s" owner name hash)

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

let status_sep = String.make 1 Common.status_sep

let build_with_docker ~analysis source =
  let* analysis = analysis in
  let pkgs = Analyse.Analysis.opam_files analysis in
  let build ~revdeps docker name variant builds =
    List.fold_left begin fun builds pkg ->
      let (module D : S.DOCKER_CONTEXT with
            type source = [ `Git of Current_git.Commit.t Current.t | `No_context ]) = docker in
      let module B = Opam_build.Make (D) in
      let image = B.v ~schedule:weekly ~variant ~pkg source in
      let revdeps =
        if revdeps then begin
          let prefix = pkg^status_sep^name^status_sep^"revdeps" in
          let+ revdeps = D.pread image ~args:["opam";"list";"-s";"--color=never";"--depends-on";pkg;"--installable";"--all-versions";"--depopts"] in
          String.split_on_char '\n' revdeps |>
          List.filter (fun pkg -> not (String.equal pkg "")) |>
          List.map begin fun pkg ->
            let image = B.v ~schedule:weekly ~variant ~pkg source in
            let build_result = Current.map (fun _ -> `Built) image in
            (prefix^status_sep^pkg, (build_result, job_id build_result))
          end
        end else begin
          Current.return []
        end
      in
      let build_result = Current.map (fun _ -> `Built) image in
      let+ builds = builds
      and+ revdeps = revdeps in
      builds @ (pkg^status_sep^name, (build_result, job_id build_result)) :: revdeps
    end builds pkgs
  in
  Current.return [] |>
  build ~revdeps:true (module Conf.Builder_amd1) "4.10" "debian-10-ocaml-4.10" |>
  build ~revdeps:true (module Conf.Builder_amd2) "4.09" "debian-10-ocaml-4.09" |>
  build ~revdeps:true (module Conf.Builder_amd3) "4.08" "debian-10-ocaml-4.08" |>
  build ~revdeps:true (module Conf.Builder_amd1) "4.07" "debian-10-ocaml-4.07" |>
  build ~revdeps:true (module Conf.Builder_amd2) "4.06" "debian-10-ocaml-4.06" |>
  build ~revdeps:true (module Conf.Builder_amd3) "4.05" "debian-10-ocaml-4.05" |>
  build ~revdeps:true (module Conf.Builder_amd1) "4.04" "debian-10-ocaml-4.04" |>
  build ~revdeps:true (module Conf.Builder_amd2) "4.03" "debian-10-ocaml-4.03" |>
  build ~revdeps:true (module Conf.Builder_amd3) "4.02" "debian-10-ocaml-4.02"

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

let get_prs repo =
  let* refs = Github.Api.Repo.ci_refs repo in
  List.fold_left begin fun acc head ->
    let+ acc = acc in
    match Github.Api.Commit.kind head with
    | `Ref _ -> acc (* Skip branches, only check PRs *)
    | `PR _ -> head :: acc
  end (Current.return []) refs

let local_test repo () =
  let src = Git.Local.head_commit repo in
  let analysis = Analyse.examine src in
  Current.component "summarise" |>
  let** result =
    let* builds = build_with_docker ~analysis (`Git src) in
    builds
    |> List.map (fun (variant, (build, _job)) -> variant, build)
    |> summarise
  in
  Current.of_output result

let v ~app () =
  Github.App.installations app |> Current.list_iter ~pp:Github.Installation.pp @@ fun installation ->
  let repos = Github.Installation.repositories installation in
  repos |> Current.list_iter ~pp:Github.Api.Repo.pp @@ fun repo ->
  let prs = get_prs repo |> set_active_refs ~repo in
  prs |> Current.list_iter ~pp:Github.Api.Commit.pp @@ fun head ->
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  let analysis = Analyse.examine src in
  let builds =
    build_with_docker ~analysis (`Git src) in
  let jobs =
    let* builds = builds in
    builds
    |> List.map (fun (variant, (_build, job)) ->
      let+ x = job in
      (variant, x)
    )
    |> Current.list_seq
  in
  let summary =
    let* builds = builds in
    builds
    |> List.map (fun (variant, (build, _job)) -> variant, build)
    |> summarise
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
    and+ analysis = job_id analysis
    and+ jobs = jobs
    and+ status = status in
    let repo = Current_github.Api.Commit.repo_id commit in
    let hash = Current_github.Api.Commit.hash commit in
    Index.record ~repo ~hash ~status @@ ("(analysis)", analysis) :: jobs
  and set_github_status =
    let* builds = builds in
    builds
    |> List.map (fun (variant, (build, _job)) -> variant, build)
    |> summarise
    |> github_status_of_state ~head
    |> Github.Api.Commit.set_status head "opam-ci"
  in
  Current.all [index; set_github_status]
