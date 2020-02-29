let profile =
  match Sys.getenv_opt "CI_PROFILE" with
  | Some "production" -> `Production
  | Some "dev" | None -> `Dev
  | Some x -> Fmt.failwith "Unknown $PROFILE setting %S" x

module Capnp = struct
  (* Cap'n Proto RPC is enabled by passing --capnp-public-address. These values are hard-coded
     (because they're just internal to the Docker container). *)

  let cap_secrets =
    match profile with
    | `Production -> "/capnp-secrets"
    | `Dev -> "./capnp-secrets"

  let secret_key = cap_secrets ^ "/secret-key.pem"
  let cap_file = cap_secrets ^ "/ocaml-ci-admin.cap"
  let internal_port = 9000
end

let dev_pool = Current.Pool.create ~label:"docker" 1

module Builder(C : sig val docker_context : string end) :
  Ocaml_ci.S.DOCKER_CONTEXT with type source = Current_docker.S.source =
struct

  module Docker = Current_docker.Make(struct
      let docker_context =
        match profile with
        | `Production -> Some C.docker_context
        | `Dev -> None
    end)

  (** Limit number of concurrent builds. *)
  let pool =
    match profile with
    | `Production -> Current.Pool.create ~label:("docker-" ^ C.docker_context) 20
    | `Dev -> dev_pool

  (** Maximum time for one Docker build. *)
  let build_timeout = Duration.of_hour 1

  type source = Current_docker.S.source

  type image = Docker.Image.t

  let image_hash = Docker.Image.hash

  let pull ~schedule name =
    Docker.pull ~schedule name

  let build ?label ~dockerfile source =
    Docker.build ~timeout:build_timeout ~pool ?label ~pull:false ~dockerfile source

  let run ?label image ~args =
    Docker.run ?label ~pool image ~args

end

module Builder_amd1 = Builder(struct let docker_context = "default" end)
module Builder_amd2 = Builder(struct let docker_context = "default" end)
module Builder_amd3 = Builder(struct let docker_context = "default" end)
