module Docker = Current_docker.Default

let crunch_list items = Dockerfile.(crunch (empty @@@ items))

let safe_char = function
  | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' -> true
  | _ -> false

let check_safe s =
  if not (Astring.String.for_all safe_char s) then
    Fmt.failwith "Unsafe characters in %S" s

let build_cache repo =
  let { Current_github.Repo_id.owner; name } = repo in
  check_safe owner;
  check_safe name;
  Printf.sprintf
    "--mount=type=cache,target=/src/_build,uid=1000,sharing=private,id=dune:%s:%s"
    owner name

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

let download_cache = "--mount=type=cache,target=/home/opam/.opam/download-cache,uid=1000"

let dockerfile ~base ~info ~repo =
  let opam_files = Analyse.Analysis.opam_files info in
  if opam_files = [] then failwith "No opam files found!";
  let groups = group_opam_files opam_files in
  let caches =
    if Analyse.Analysis.is_duniverse info then Printf.sprintf "%s %s" download_cache (build_cache repo)
    else download_cache
  in
  let dirs = groups |> List.map (fun (dir, _) -> Printf.sprintf "%S" (Fpath.to_string dir)) |> String.concat " " in
  let open Dockerfile in
  comment "syntax = docker/dockerfile:experimental" @@
  from (Docker.Image.hash base) @@
  workdir "/src" @@
  run "sudo chown opam /src" @@
  pin_opam_files groups @@
  run "%s opam install %s --show-actions --deps-only -t | awk '/- install/{print $3 \".\" $4}' | xargs opam depext -iy" download_cache dirs @@
  copy ~chown:"opam" ~src:["."] ~dst:"/src/" () @@
  run "%s opam install -tv ." caches
