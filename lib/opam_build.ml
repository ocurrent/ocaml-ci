let download_cache = "--mount=type=cache,target=/home/opam/.opam/download-cache,uid=1000"

type key =  {
  base : string;
  pkg : string;
  variant : string;
}

let dockerfile {base; pkg; variant} =
  let open Dockerfile in
  let distro_extras =
    if Astring.String.is_prefix ~affix:"fedora" variant then
      run "sudo dnf install -y findutils" (* (we need xargs) *)
    else
      empty
  in
  comment "syntax = docker/dockerfile:experimental@sha256:ee85655c57140bd20a5ebc3bb802e7410ee9ac47ca92b193ed0ab17485024fe5" @@
  from base @@
  comment "%s" variant @@
  distro_extras @@
  copy ~chown:"opam" ~src:["."] ~dst:"/src/" () @@
  workdir "/src" @@
  run "git checkout -b cibranch && git checkout master && git merge cibranch && opam repository set-url default file:///src" @@
  run "%s opam depext -ivy %s" download_cache pkg

let cache = Hashtbl.create 10000
let cache_max_size = 1000000

let dockerfile ~base ~pkg ~variant =
  let key = { base; pkg; variant } in
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

  let v ~pkg source {base; variant} =
    let dockerfile =
      let open Current.Syntax in
      let+ base = base in
      `Contents (dockerfile ~base:(Docker.image_hash base) ~pkg ~variant)
    in
    Docker.build ~dockerfile source
end
