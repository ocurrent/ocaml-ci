let crunch_list items = Dockerfile.(crunch (empty @@@ items))

(* If the package's directory name doesn't contain a dot then opam will default to
   using the last known version, which is usually wrong. In particular, if a multi-project
   repostory adds a new package with a constraint "{ =version }" on an existing one,
   this will fail because opam will pin the new package as "dev" but the old one with
   the version of its last release. *)
let maybe_add_dev ~dir name =
  if Fpath.is_current_dir dir || not (String.contains (Fpath.basename dir) '.') then name ^ ".dev" else name

(* Group opam files by directory.
   e.g. ["a/a1.opam"; "a/a2.opam"; "b/b1.opam"] ->
        [("a", ["a/a1.opam"; "a/a2.opam"], ["a1.dev"; "a2.dev"]);
         ("b", ["b/b1.opam"], ["b1.dev"])
        ] *)
let group_opam_files =
  ListLabels.fold_left ~init:[] ~f:(fun acc x ->
      let item = Fpath.v x in
      let dir = Fpath.parent item in
      let pkg = Filename.basename x |> Filename.chop_extension |> maybe_add_dev ~dir in
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

(* Get the packages directly in "." *)
let rec get_root_opam_packages = function
  | [] -> []
  | (dir, _, pkgs) ::_ when Fpath.is_current_dir dir -> pkgs
  | _ :: rest -> get_root_opam_packages rest

let download_cache = "--mount=type=cache,target=/home/opam/.opam/download-cache,uid=1000"

let dockerfile ~base ~info ~variant ~for_user =
  let opam_files = Analyse.Analysis.opam_files info in
  let groups = group_opam_files opam_files in
  let root_pkgs = get_root_opam_packages groups in
  let download_cache_prefix = if for_user then "" else download_cache ^ " " in
  let open Dockerfile in
  let distro_extras =
    if Astring.String.is_prefix ~affix:"fedora" variant then
      run "sudo dnf install -y findutils" (* (we need xargs) *)
    else
      empty
  in
  let build_cmd =
    copy ~chown:"opam" ~src:["."] ~dst:"/src/" () @@
    run "opam exec -- dune build @install @runtest && rm -rf _build"
  in
  (if for_user then empty
   else comment "syntax = docker/dockerfile:experimental@sha256:ee85655c57140bd20a5ebc3bb802e7410ee9ac47ca92b193ed0ab17485024fe5") @@
  from base @@
  comment "%s" variant @@
  distro_extras @@
  workdir "/src" @@
  run "sudo chown opam /src" @@
  pin_opam_files groups @@
  run "(opam install %s --dry-run --deps-only -ty; echo $? > /tmp/exit-status) | tee /tmp/opam-plan; exit $(cat /tmp/exit-status)" (root_pkgs |> String.concat " ") @@
  run "%sawk < /tmp/opam-plan '/-> installed/{print $3}' | xargs opam depext --update -iy" download_cache_prefix @@
  crunch_list (List.map (fun pkg ->
      run {|test "$(opam show -f depexts: %s)" = "$(printf "\n")" || opam depext -ty %s|} pkg pkg) root_pkgs
    ) @@
  (if Analyse.Analysis.is_duniverse info then run "opam pin remove $(opam pin -s) -n" else empty) @@
  build_cmd
