let latest_patch_release_with_flambda compiler =
  let open Ocaml_version in
  of_string_exn compiler |> fun ov ->
  Releases.all_patches |>
  List.filter (fun ov' -> with_just_major_and_minor ov' = ov) |>
  List.sort compare |> List.rev |> function
  | [] -> failwith ("Unknown compiler release" ^ compiler)
  | hd::_ -> Opam.V2.name @@ with_variant hd (Some "musl+flambda") (* TODO add `Musl to ocaml-version *)

let install_bin ~compiler ~repo ~tag ~bins name =
  let open Dockerfile in
  let alias = Printf.sprintf "%s_binary" name in
  let base = Printf.sprintf "ocurrent/opam:debian-10-ocaml-%s" compiler in
  let switch = latest_patch_release_with_flambda compiler in
  let build =
    from ~alias base @@
    run "sudo apt-get -y install musl-tools" @@
    run "opam switch create static %s" switch @@ (* TODO base-compilers should build this variant *)
    run "opam install -y dune" @@
    run "git clone --depth=1 --branch=%s https://github.com/%s.git /home/opam/src" tag repo @@
    workdir "/home/opam/src" @@
    run "echo \"(lang dune 2.0)\" > dune-workspace" @@
    run "echo \"(env (_ (flags -cclib -static)))\" >> dune-workspace" @@
    run "opam pin add -n ." @@
    run "opam depext %s" name @@
    run "opam install --deps-only %s" name @@
    run "opam exec -- dune build @install" @@@
    (List.map (fun (bin, target) ->
       run "sudo install -m 0755 -o root _build/install/default/%s %s" bin target) bins)
  in
  let use = copy ~from:alias ~src:(List.map snd bins) ~dst:"/usr/bin/" () in
  build, use

let install_platform ~compiler =
  let duniverse = install_bin ~compiler
    ~repo:"ocamllabs/duniverse" ~tag:"dev-ocaml-ci-3"
    ~bins:["bin/duniverse", "/usr/bin/duniverse"] "duniverse" in
  let dune = install_bin ~compiler
    ~repo:"ocaml/dune" ~tag:"2.5.0"
    ~bins:["bin/dune", "/usr/bin/dune"] "dune" in
  let odoc = install_bin ~compiler
    ~repo:"ocaml/odoc" ~tag:"1.5.0"
    ~bins:["bin/odoc", "/usr/bin/odoc"] "odoc" in
  let ocamlformat = install_bin ~compiler
    ~repo:"ocaml-ppx/ocamlformat" ~tag:"0.14.0"
    ~bins:["bin/ocamlformat", "/usr/bin/ocamlformat"] "ocamlformat" in
  let open Dockerfile in
  List.fold_left (fun (b, u) (b',u') -> (b @@ b'), (u @@ u')) (empty, empty)
    [ duniverse; dune; odoc; ocamlformat ]

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

let dockerfile ~base ~repo ~variant ~for_user =
  let caches =
    if for_user then ""
    else Printf.sprintf "%s %s" download_cache (build_cache repo)
  in
  let build_platform, install_platform = install_platform ~compiler:"4.09" in
  let open Dockerfile in
  (if for_user then empty
   else comment "syntax = docker/dockerfile:experimental@sha256:ee85655c57140bd20a5ebc3bb802e7410ee9ac47ca92b193ed0ab17485024fe5") @@
  build_platform @@
  from base @@
  install_platform @@
  comment "%s" variant @@
  workdir "/src" @@
  run "sudo chown opam /src" @@
  copy ~chown:"opam" ~src:["dune-get"] ~dst:"/src/" () @@
  (* TODO make duniverse depext install the package as opam-depext does *)
  run "sudo apt-get update && sudo apt-get -y install build-essential `duniverse depext`" @@
  run "duniverse pull" @@
  copy ~chown:"opam" ~src:["."] ~dst:"/src/" () @@
  run "%s opam exec -- dune build @install" caches @@
  run "%s opam exec -- dune runtest" caches @@
  run "%s opam exec -- dune build @doc" caches
