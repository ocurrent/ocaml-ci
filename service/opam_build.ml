(*let download_cache = "--mount=type=cache,target=/home/opam/.opam/download-cache,uid=1000"*)
(* TODO: Find the way to have this? *)

type key =  {
  base : string;
  pkg : string;
  variant : string;
}

let dockerfile { base; pkg; variant} =
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
  run "git -C /home/opam/opam-repository pull origin master && git -C /home/opam/opam-repository pull /src && opam update default" @@
  run "opam depext -ivy %s" pkg

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
