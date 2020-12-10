(* Hopefully replace this will a package *)

module OsVars = Ocaml_ci_api.Worker.Vars
module DD = Dockerfile_distro


let distros = [
  `Debian `V10; `Alpine `V3_12;
  `Ubuntu `V18_04; `OpenSUSE `V15_2; `CentOS `V8;
  `Fedora `V32 ] 

let vars ov = 
  let get_version distro = 
    let s = DD.human_readable_string_of_distro distro in 
      match Astring.String.cuts ~sep:" " s with 
        | _::v::_ -> v
        | _ -> failwith "Distro Version Mangled"
  in
  let linux = List.map (fun distro -> 
    List.map (fun arch -> { 
      OsVars.arch = Ocaml_version.to_opam_arch arch;
      os = "linux";
      os_family = DD.human_readable_short_string_of_distro distro |> String.lowercase_ascii;
      os_distribution = DD.human_readable_short_string_of_distro distro |> String.lowercase_ascii;
      os_version = get_version distro;
      ocaml_package = "ocaml";
      ocaml_version = Ocaml_version.to_string ov;
      }) 
    [ `X86_64 ])
  distros |> List.flatten in 
    linux @ [
      {
        OsVars.arch = Ocaml_version.to_opam_arch `X86_64;
        os = "macos";
        os_family = "macos";
        os_distribution = "homebrew";
        os_version = "latest";
        ocaml_package = "ocaml";
        ocaml_version = Ocaml_version.to_string ov;
      };
      {
        OsVars.arch = Ocaml_version.to_opam_arch `X86_64;
        os = "win32";
        os_family = "win32";
        os_distribution = "windows";
        os_version = "latest";
        ocaml_package = "ocaml";
        ocaml_version = Ocaml_version.to_string ov;
      }
    ]