module OV = Ocaml_version
module OVC = OV.Configure_options

let dockerfile ~base ~variant ~for_user =
  let ov = Variant.ocaml_version variant in
  let opts = OVC.of_t ov |> Rresult.R.get_ok |> List.map (OVC.to_configure_flag ov) |> String.concat " " in
  let open Dockerfile in
  (if for_user then empty else Buildkit_syntax.add (Variant.arch variant)) @@
  from base @@
  (if Variant.arch variant |> Ocaml_version.arch_is_32bit then
     shell ["/usr/bin/linux32"; "/bin/sh"; "-c"] else empty) @@
  comment "%s" (Variant.to_string variant) @@
  workdir "/src" @@
  run "sudo chown opam /src" @@
  copy ~chown:"opam" ~src:["."] ~dst:"/src/" () @@
  run "./configure %s" opts @@
  run "make -j world.opt" @@
  run "make tests"
