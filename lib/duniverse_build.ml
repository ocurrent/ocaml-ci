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

let download_cache = "--mount=type=cache,target=/home/opam/.opam/download-cache,uid=1000"

let install_opam_tools =
  let opam_tools_hash = "6c56ab9fedd7b3f6c143cb606a0ea6fe6a384013" in
  let open Dockerfile in
     run "opam pin add -n https://github.com/avsm/opam-tools.git#%s" opam_tools_hash
  @@ run "opam depext -iy opam-tools"

let dockerfile ~base ~repo ~variant ~for_user =
  let caches =
    if for_user then ""
    else Printf.sprintf "%s %s" download_cache (build_cache repo)
  in
  let open Dockerfile in
  (if for_user then empty else Buildkit_syntax.add (Variant.arch variant)) @@
  from base @@
  comment "%s" (Variant.to_string variant) @@
  install_opam_tools @@
  workdir "/src" @@
  run "sudo chown opam /src" @@
  copy ~chown:"opam" ~src:["*.opam"] ~dst:"/src/" () @@
  run "opam tools --no-install --compiler `opam exec -- ocamlc -version` -vv" @@
  copy ~chown:"opam" ~src:["dune-get"] ~dst:"/src/" () @@
  (* TODO make duniverse depext install the package as opam-depext does *)
  run "sudo apt-get update && sudo apt-get -y install build-essential `opam exec -- duniverse depext`" @@
  run "opam exec -- duniverse pull" @@
  copy ~chown:"opam" ~src:["."] ~dst:"/src/" () @@
  run "%s opam exec -- dune build @install" caches @@
  run "%s opam exec -- dune runtest" caches
