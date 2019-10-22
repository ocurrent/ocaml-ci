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
        [("a", ["a/a1.opam"; "a/a2.opam"], ["a1.dev"; "a2.dev"]);
         ("b", ["b/b1.opam"], ["b1.dev"])
        ] *)
let group_opam_files =
  ListLabels.fold_left ~init:[] ~f:(fun acc x ->
      let item = Fpath.v x in
      let dir = Fpath.parent item in
      let pkg = (Filename.basename x |> Filename.chop_extension) ^ ".dev" in
      match acc with
      | (prev_dir, prev_items, pkgs) :: rest when Fpath.equal dir prev_dir -> (prev_dir, x :: prev_items, pkg :: pkgs) :: rest
      | _ -> (dir, [x], [pkg]) :: acc
    )

(* Generate Dockerfile instructions to copy all the files in [items] into the
   image, creating the necessary directories first, and then pin them all. *)
let pin_opam_files groups =
  let open Dockerfile in
  let dirs = groups |> List.map (fun (dir, _, _) -> Printf.sprintf "%S" (Fpath.to_string dir)) |> String.concat " " in
  (run "mkdir -p %s" dirs @@@ (
    groups |> List.map (fun (dir, files, _) ->
        copy ~src:files ~dst:(Fpath.to_string dir) ()
      )
  )) @@ crunch_list (
    groups |> List.map (fun (dir, _, pkgs) ->
        pkgs
        |> List.map (fun pkg ->
            run "opam pin add -yn %s %S" pkg (Fpath.to_string dir)
          )
        |> crunch_list
      )
  )

let get_opam_packages = List.fold_left (fun acc (_, _, pkgs) -> pkgs @ acc) []

let download_cache = "--mount=type=cache,target=/home/opam/.opam/download-cache,uid=1000"

let dockerfile ~base ~info ~repo =
  let opam_files = Analyse.Analysis.opam_files info in
  if opam_files = [] then failwith "No opam files found!";
  let groups = group_opam_files opam_files in
  let caches =
    if Analyse.Analysis.is_duniverse info then Printf.sprintf "%s %s" download_cache (build_cache repo)
    else download_cache
  in
  let pkgs = get_opam_packages groups in
  let open Dockerfile in
  comment "syntax = docker/dockerfile:experimental" @@
  from base @@
  workdir "/src" @@
  run "sudo chown opam /src" @@
  pin_opam_files groups @@
  run "(opam install %s --dry-run --deps-only -ty; echo $? > /tmp/exit-status) | tee /tmp/opam-plan; exit $(cat /tmp/exit-status)" (pkgs |> String.concat " ") @@
  run "%s awk < /tmp/opam-plan '/-> installed/{print $3}' | xargs opam depext -iy" download_cache @@
  crunch_list (List.map (fun pkg ->
      run {|test "$(opam show -f depexts: %s)" = "$(printf "\n")" || opam depext -ty %s|} pkg pkg) pkgs
    ) @@
  copy ~chown:"opam" ~src:["."] ~dst:"/src/" () @@
  run "%s opam install -tv ." caches
