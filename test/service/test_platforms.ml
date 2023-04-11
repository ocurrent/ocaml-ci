let debian_11_vars ocaml_package ocaml_version =
  {
    Ocaml_ci_api.Worker.Vars.os = "debian";
    arch = "x86_64";
    os_family = "debian";
    os_distribution = "debian";
    os_version = "11";
    opam_version = "2.1.3";
    ocaml_package;
    ocaml_version;
  }

let var distro ov =
  Obuilder_spec_opam.Variant.v ~arch:`X86_64 ~distro
    ~ocaml_version:(Ocaml_version.of_string_exn ov)
    ~opam_version:`V2_1
  |> function
  | Ok v -> v
  | Error (`Msg m) -> failwith m

let v =
  [
    (var "debian-11" "4.10", debian_11_vars "ocaml" "4.10.0");
    (var "debian-11" "4.09", debian_11_vars "ocaml" "4.09.0");
    (var "debian-11" "4.08", debian_11_vars "ocaml" "4.08.0");
    (var "debian-11" "4.07", debian_11_vars "ocaml" "4.07.0");
  ]
