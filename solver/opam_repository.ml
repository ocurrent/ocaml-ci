open Lwt.Infix

type t = {
  dir : string;
}

let ( / ) = Filename.concat

let of_dir dir = { dir }

let packages_dir t = t.dir / "packages"

let oldest_commit_with t pkgs =
  let paths =
    pkgs |> List.map (fun pkg ->
        let name = OpamPackage.name_to_string pkg in
        let version = OpamPackage.version_to_string pkg in
        Printf.sprintf "%s/%s.%s" name name version
      )
  in
  let cmd = "git" :: "-C" :: packages_dir t :: "log" :: "-n" :: "1" :: "--format=format:%H" :: paths in
  let cmd = ("", Array.of_list cmd) in
  Process.pread cmd >|= String.trim
