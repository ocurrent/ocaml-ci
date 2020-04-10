let download_cache = "--mount=type=cache,target=/home/opam/.opam/download-cache,uid=1000"

type key =  {
  base : string;
  variant : string;
  pkg : string;
  revdep : string option;
  with_tests : bool;
}

let opam_install ~with_tests ~pkg =
  let open Dockerfile in
  let version =
    let idx = String.index pkg '.' + 1 in
    String.sub pkg idx (String.length pkg - idx)
  in
  run "opam pin add -k version -yn %s %s" pkg version @@
  run "%s opam depext -uivy%s %s" download_cache (if with_tests then "t" else "") pkg

let dockerfile {base; variant; pkg; revdep; with_tests} =
  let open Dockerfile in
  let distro_extras =
    if Astring.String.is_prefix ~affix:"fedora" variant then
      run "sudo dnf install -y findutils" (* (we need xargs) *)
    else
      empty
  in
  let opam_extras =
    if Astring.String.is_suffix ~affix:"-ocaml-4.06" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.05" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.04" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.03" variant ||
       Astring.String.is_suffix ~affix:"-ocaml-4.02" variant then
      run "%s opam install -y ocaml-secondary-compiler" download_cache (* Speed up builds for dune >= 2.0 *)
    else
      empty
  in
  let revdep = match revdep with
    | None -> empty
    | Some revdep -> opam_install ~with_tests:false ~pkg:revdep
  and tests = match with_tests, revdep with
    | true, None -> opam_install ~with_tests:true ~pkg
    | true, Some revdep -> opam_install ~with_tests:true ~pkg:revdep
    | false, _ -> empty
  in
  comment "syntax = docker/dockerfile:experimental@sha256:ee85655c57140bd20a5ebc3bb802e7410ee9ac47ca92b193ed0ab17485024fe5" @@
  from base @@
  distro_extras @@
  opam_extras @@
  copy ~chown:"opam" ~src:["."] ~dst:"/src/" () @@
  workdir "/src" @@
  run "git checkout -b opam-ci__cibranch origin/master && git merge master && opam repository set-url --strict default file:///src" @@
  opam_install ~with_tests:false ~pkg @@
  revdep @@
  tests

let cache = Hashtbl.create 10000
let cache_max_size = 1000000

let dockerfile ~base ~revdep ~with_tests ~pkg ~variant =
  let key = { base; variant; pkg; revdep; with_tests } in
  match Hashtbl.find_opt cache key with
  | Some x -> x
  | None ->
    let x = dockerfile key in
    if Hashtbl.length cache > cache_max_size then Hashtbl.clear cache;
    Hashtbl.add cache key x;
    x

module Make (Docker : S.DOCKER_CONTEXT) = struct
  type t = {
    base : Docker.image Current.t;
    variant : string;
  }

  let base ~schedule ~variant = {
    base = Docker.pull ~schedule ("ocurrent/opam:" ^ variant);
    variant;
  }

  let v ~revdep ~with_tests ~pkg source {base; variant} =
    let dockerfile =
      let open Current.Syntax in
      let+ base = base in
      `Contents (dockerfile ~base:(Docker.image_hash base) ~revdep ~with_tests ~pkg ~variant)
    in
    Docker.build ~dockerfile source
end
