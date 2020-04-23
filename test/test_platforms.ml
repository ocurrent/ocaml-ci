let debian_10_vars = { Ocaml_ci.Platform.Vars.
                       os = "debian";
                       arch = "x86_64";
                       os_family = "debian";
                       os_distribution = "debian";
                       os_version = "10" }
let v = [
  "debian-10-ocaml-4.10", debian_10_vars;
  "debian-10-ocaml-4.09", debian_10_vars;
  "debian-10-ocaml-4.08", debian_10_vars;
  "debian-10-ocaml-4.07", debian_10_vars;
]
