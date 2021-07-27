(* From `docker manifest inspect docker/dockerfile:experimental` *)
let hash_for = function
  |`X86_64 -> "sha256:8c69d118cfcd040a222bea7f7d57c6156faa938cb61b47657cd65343babc3664"
  |`I386 -> "sha256:8c69d118cfcd040a222bea7f7d57c6156faa938cb61b47657cd65343babc3664"
  |`Aarch64 -> "sha256:d9ced99b409ddb781c245c7c11f72566f940424fc3883ac0b5c5165f402e5a09"
  |`Aarch32 -> "sha256:5f502d5a34f8cd1780fde9301b69488e9c0cfcecde2d673b6bff19aa4979fdfc"
  |`Ppc64le -> "sha256:c0fe20821d527e147784f7e782513880bf31b0060b2a7da7a94582ecde81c85f"
  |`S390x -> "sha256:e2b9c21cc1d0067116c572db562f80de9b0c7a654ac41f094651a724408beafc"

let add arch =
  let hash = hash_for (match arch with
    | `X86_64 | `I386 -> `X86_64
    | `Aarch64 | `Aarch32 -> `Aarch64
    | `Ppc64le -> `Ppc64le
    | `S390x -> `S390x) in
  Printf.sprintf "# syntax = docker/dockerfile:experimental@%s\n" hash
