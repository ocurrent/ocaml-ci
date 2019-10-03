module Docker = Current_docker.Default

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

let download_cache = "--mount=type=cache,target=/home/opam/.opam/download-cache,uid=1000"

let dockerfile ~base ~info =
  let opam_files = Analyse.Analysis.opam_files info in
  if opam_files = [] then failwith "No opam files found!";
  let groups = group_opam_files opam_files in
  let dirs = groups |> List.map (fun (dir, _) -> Printf.sprintf "%S" (Fpath.to_string dir)) |> String.concat " " in
  let open Dockerfile in
  comment "syntax = docker/dockerfile:experimental" @@
  from (Docker.Image.hash base) @@
  workdir "/src" @@
  run "sudo chown opam /src" @@
  pin_opam_files groups @@
  run "%s opam install %s --show-actions --deps-only -t | awk '/- install/{print $3}' | xargs opam depext -iy" download_cache dirs @@
  copy ~chown:"opam" ~src:["."] ~dst:"/src/" () @@
  run "%s opam install -tv ." download_cache
