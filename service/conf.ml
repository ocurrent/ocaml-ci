let profile =
  match Sys.getenv_opt "CI_PROFILE" with
  | Some "production" -> `Production
  | Some "dev" | None -> `Dev
  | Some x -> Fmt.failwith "Unknown $PROFILE setting %S" x

module Capnp = struct
  (* Cap'n Proto RPC is enabled by passing --capnp-public-address. These values are hard-coded
     (because they're just internal to the Docker container). *)
  let secret_key = "/capnp-secrets/secret-key.pem"
  let cap_file = "/capnp-secrets/ocaml-ci-admin.cap"
  let internal_port = 9000
end
