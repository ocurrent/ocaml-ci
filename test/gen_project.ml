type contents = Format.formatter -> unit

let opam_monorepo_spec_file ppf =
  Fmt.pf ppf
      {|
opam-version: "2.0"
synopsis: "spec file"
maintainer: "opam-monorepo"
depends: [
  "dune" {= "1.0"}
  "ocaml" {= "4.10.0"}
] |}

let opam_monorepo_lock_file ~monorepo_version ppf =
  let pp_version_field ppf s =
    Fmt.pf ppf "x-opam-monorepo-version: \"%s\"\n" s
  in
  Fmt.pf ppf
    {|
opam-version: "2.0"
synopsis: "opam-monorepo generated lockfile"
maintainer: "opam-monorepo"
depends: [
  "dune" {= "1.0"}
  "ocaml" {= "4.10.0"}
]
%a
x-opam-monorepo-root-packages: [ "test-opam-monorepo" ]
x-opam-monorepo-duniverse-dirs: [ ]
    |}
    (Fmt.option pp_version_field) monorepo_version


let dummy_opam ppf =
  Fmt.pf ppf
    {|opam-version: "2.0"
maintainer:   "Camelus Bactrianus"
authors:      ["Camelus Bactrianus"]
license:      "ISC"
homepage:     "https://www.example.com"
bug-reports:  "https://www.example.com/issues"
dev-repo:     "git+https://example.com/repo.git"
build: []
depends: []
synopsis: "Example project generated for testing purposes"
|}

let opam ppf =
  Fmt.pf ppf
    {|opam-version: "2.0"
maintainer:   "Camelus Bactrianus"
authors:      ["Camelus Bactrianus"]
license:      "ISC"
homepage:     "https://www.example.com"
bug-reports:  "https://www.example.com/issues"
dev-repo:     "git+https://example.com/repo.git"

build: [
 ["dune" "subst"] {pinned}
 ["dune" "build" "-p" name "-j" jobs]
 ["dune" "runtest" "-p" name] {with-test}
]

depends: [
  "ocaml"   {>= "4.09"}
  "dune"
  "fmt"
  "logs"
  "alcotest" {with-test}
]

synopsis: "Example project generated for testing purposes"
|}

let ocamlformat ~version ppf =
  Fmt.pf ppf {|version = %s
profile = conventional
  |} version

let empty_file _ppf = ()

(* Project generation logic *)

type file = Folder of string * file list | File of string * contents

let folder name items = Folder (name, items)

let file name content = File (name, content)

let print_to_file path printer =
  let channel = open_out path in
  let formatter = Format.formatter_of_out_channel channel in
  printer formatter;
  Format.pp_print_newline formatter ();
  close_out channel

let rec mkdir_p path =
  try Unix.mkdir path 0o777 with
  | Unix.Unix_error (EEXIST, _, _) -> ()
  | Unix.Unix_error (ENOENT, _, _) ->
      let parent = Filename.dirname path in
      mkdir_p parent;
      Unix.mkdir path 0o777

let rec instantiate ~root =
  mkdir_p root;
  List.iter (function
    | Folder (name, contents) ->
        mkdir_p name;
        instantiate ~root:(Filename.concat root name) contents
    | File (name, printer) -> print_to_file (Filename.concat root name) printer)

let dummy_package name versions =
  folder name
    ( versions
    |> List.map (fun version ->
           folder
             (Printf.sprintf "%s.%s" name version)
             [ file "opam" dummy_opam ]) )
