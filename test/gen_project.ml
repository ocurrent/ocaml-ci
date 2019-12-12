type contents = Format.formatter -> unit

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
