let debian_10_vars ocaml_package ocaml_version =
  {
    Ocaml_ci_api.Worker.Vars.os = "debian";
    arch = "x86_64";
    os_family = "debian";
    os_distribution = "debian";
    os_version = "10";
    opam_version = "2.0.10";
    ocaml_package;
    ocaml_version;
  }

let var distro ov =
  Ocaml_ci.Variant.v ~arch:`X86_64 ~distro
    ~ocaml_version:(Ocaml_version.of_string_exn ov)
    ~opam_version:`V2_0
  |> function
  | Ok v -> v
  | Error (`Msg m) -> failwith m

let v =
  [
    (var "debian-10" "4.10", debian_10_vars "ocaml" "4.10.0");
    (var "debian-10" "4.09", debian_10_vars "ocaml" "4.09.0");
    (var "debian-10" "4.08", debian_10_vars "ocaml" "4.08.0");
    (var "debian-10" "4.07", debian_10_vars "ocaml" "4.07.0");
  ]
