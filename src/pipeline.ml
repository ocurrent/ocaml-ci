open Current.Syntax

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

(* Limit number of concurrent builds. *)
let pool = Lwt_pool.create 20 Lwt.return

(* Link for GitHub statuses. *)
let url = Uri.of_string "https://ci.ocamllabs.io:8100/"

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let crunch_list items = Dockerfile.(crunch (empty @@@ items))

(* Group opam files by directory.
   e.g. ["a/a1.opam"; "a/a2.opam"; "b/b1.opam"] ->
        [("a", ["a/a1.opam"; "a/a2.opam"]);
         ("b", ["b/b1.opam"])
        ] *)
let group_opam_files =
  ListLabels.fold_left ~init:[] ~f:(fun acc x ->
      let item = Fpath.v x in
      let dir = Fpath.parent item in
      match acc with
      | (prev_dir, prev_items) :: rest when Fpath.equal dir prev_dir -> (prev_dir, x :: prev_items) :: rest
      | _ -> (dir, [x]) :: acc
    )

(* Generate Dockerfile instructions to copy all the files in [items] into the
   image, creating the necessary directories first, and then pin them all. *)
let pin_opam_files groups =
  let open Dockerfile in
  let dirs = groups |> List.map (fun (dir, _) -> Printf.sprintf "%S" (Fpath.to_string dir)) |> String.concat " " in
  (run "mkdir -p %s" dirs @@@ (
    groups |> List.map (fun (dir, files) ->
        copy ~src:files ~dst:(Fpath.to_string dir) ()
      )
  )) @@ crunch_list (
    groups |> List.map (fun (dir, files) ->
        files
        |> List.map (fun file ->
            run "opam pin add -yn %s.dev %S" (Filename.basename file |> Filename.chop_extension) (Fpath.to_string dir)
          )
        |> crunch_list
      )
  )

(* Generate a Dockerfile for building all the opam packages in the build context. *)
let dockerfile ~base ~opam_files =
  let groups = group_opam_files opam_files in
  let dirs = groups |> List.map (fun (dir, _) -> Printf.sprintf "%S" (Fpath.to_string dir)) |> String.concat " " in
  let open Dockerfile in
  from (Docker.Image.hash base) @@
  workdir "/src" @@
  run "sudo chown opam /src" @@
  pin_opam_files groups @@
  run "opam install %s --show-actions --deps-only -t | awk '/- install/{print $3}' | xargs opam depext -iy" dirs @@
  copy ~chown:"opam" ~src:["."] ~dst:"/src/" () @@
  run "opam install -tv ."

let github_status_of_state = function
  | Ok _              -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m)    -> Github.Api.Status.v ~url `Failure ~description:m

let v ~app () =
  Github.App.installations app |> Current.list_iter ~pp:Github.Installation.pp @@ fun installation ->
  let github = Current.map Github.Installation.api installation in
  let repos = Github.Installation.repositories installation in
  repos |> Current.list_iter ~pp:Github.Repo_id.pp @@ fun repo ->
  Github.Api.ci_refs_dyn github repo
  |> Current.list_iter ~pp:Github.Api.Commit.pp @@ fun head ->
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  let dockerfile =
    let+ base = Docker.pull ~schedule:weekly "ocurrent/opam:alpine-3.10-ocaml-4.08"
    and+ opam_files = Opam.find_opam_files src in
    dockerfile ~base ~opam_files
  in
  Docker.build ~pool ~pull:false ~dockerfile (`Git src)
  |> Current.state
  |> Current.map github_status_of_state
  |> Github.Api.Commit.set_status head "ocaml-ci"
