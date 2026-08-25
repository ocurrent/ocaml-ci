open Current.Syntax
open Capnp_rpc_lwt
open Lwt.Infix

let src =
  Logs.Src.create "ocaml_ci.cluster_build" ~doc:"ocaml-ci ocluster builder"

module Log = (val Logs.src_log src : Logs.LOG)
module Image = Current_docker.Raw.Image
module Git = Current_git

let ( >>!= ) = Lwt_result.bind

type t = {
  connection : Current_ocluster.Connection.t;
  timeout : Duration.t option;
  on_cancel : string -> unit;
  enable_day10 : bool;
      (* When set, project builds on day10-capable variants (see
         [day10_supported]) are submitted as day10 custom jobs instead of
         OBuilder jobs. Sourced from the OCAML_CI_USE_DAY10 env var. *)
}

module Op = struct
  type nonrec t = t

  let id = "ci-ocluster-build"

  module Key = struct
    type t = {
      pool : Platform.Pool_name.t;
          (* The build pool to use (e.g. "linux-arm64") *)
      commit : Current_git.Commit_id.t; (* The source code to build and test *)
      repo : Repo_id.t; (* Used to choose a build cache *)
      label : string; (* A unique ID for this build within the commit *)
    }

    let to_json { pool; commit; label; repo } =
      `Assoc
        [
          ("pool", `String (Platform.Pool_name.to_string pool));
          ("commit", `String (Current_git.Commit_id.hash commit));
          ("repo", `String (Fmt.to_to_string Repo_id.pp repo));
          ("label", `String label);
        ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = struct
    type t = {
      ty : Spec.ty;
      base : Current_docker.Raw.Image.t;
          (* The image with the OCaml compiler to use. *)
      variant : Variant.t; (* Added as a comment in the Dockerfile *)
    }

    let to_json { base; ty; variant } =
      `Assoc
        [
          ("base", `String (Image.hash base));
          ("op", Spec.ty_to_yojson ty);
          ("variant", Variant.to_yojson variant);
        ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Outcome = Current.Unit

  let hash_packages packages =
    Digest.string (String.concat "," packages) |> Digest.to_hex

  let get_cache_hint { Repo_id.owner; name } { Value.base; variant; ty } =
    let deps =
      match ty with
      | `Opam (`Build, selection, _) -> hash_packages selection.packages
      | `Opam (`Lint (`Doc | `Opam), selection, _) ->
          hash_packages selection.packages
      | `Opam_fmt (selection, _) -> "ocamlformat-" ^ selection.Selection.commit
      | `Opam_monorepo _ -> "opam-monorepo-" ^ Variant.to_string variant
    in
    Fmt.str "%s/%s-%s-%a-%s" owner name (Image.hash base) Variant.pp variant
      deps

  (* Variants routed through day10 instead of OBuilder. For now this is the
     RISC-V path: OBuilder RISC-V builds are so slow that most users don't wait
     for them, so we run them via day10 (served by the carpenter worker). The
     RISC-V variant is also marked experimental (see Build_info) so a day10
     failure does not fail the commit while the path is proven out. *)
  let day10_supported variant =
    (match Variant.os variant with `linux -> true | _ -> false)
    && (match Variant.arch variant with `Riscv64 -> true | _ -> false)

  (* Build a Custom "day10" job for a project build. The project source is
     supplied out-of-band via [~src] (the checkout); day10 solves and builds
     the dependencies against the opam-repository at [selection.commit],
     reading it from the worker's git mirror. *)
  let day10_action ~variant ~(selection : Selection.t) ~dune_args =
    (* Prefer the fully-resolved compiler version from the selection (e.g.
       "ocaml.5.3.0") over the variant's possibly major.minor version. *)
    let ocaml_version =
      match
        List.find_opt
          (fun p -> String.length p > 6 && String.sub p 0 6 = "ocaml.")
          selection.Selection.packages
      with
      | Some p -> p
      | None -> "ocaml." ^ Ocaml_version.to_string (Variant.ocaml_version variant)
    in
    let os =
      match Variant.os variant with
      | `linux -> "linux"
      | `freeBSD -> "freebsd"
      | `macOS -> "macos"
      | `windows -> "windows"
      | `openBSD -> "openbsd"
    in
    let arch = Ocaml_version.to_opam_arch (Variant.arch variant) in
    (* [Variant.distro] is the ocaml-ci distro string (e.g. "debian-13"); split
       into day10's --os-distribution and --os-version. *)
    let os_distribution, os_version =
      let d = Variant.distro variant in
      match String.rindex_opt d '-' with
      | Some i -> (String.sub d 0 i, String.sub d (i + 1) (String.length d - i - 1))
      | None -> (d, "")
    in
    let payload builder =
      let module B = Cluster_api.Raw.Builder.Day10 in
      let day10 = B.init_pointer builder in
      B.verb_set day10 "build";
      B.opam_repository_commit_set day10 selection.Selection.commit;
      B.ocaml_version_set day10 ocaml_version;
      B.os_set day10 os;
      B.arch_set day10 arch;
      B.os_distribution_set day10 os_distribution;
      B.os_version_set day10 os_version;
      B.with_test_set day10 true;
      let _ = B.dune_args_set_list day10 dune_args in
      (* The solver's per-variant compatible subset of the repo's local
         packages. Empty means all (day10's default); non-empty drops packages
         gated to a newer compiler (e.g. prometheus-eio on 4.14) instead of
         failing the whole solve. *)
      let _ = B.only_packages_set_list day10 selection.Selection.only_packages in
      ()
    in
    Cluster_api.Submission.custom_build
      (Cluster_api.Custom.v ~kind:"day10" payload)

  let run t job { Key.pool; commit; label = _; repo } spec =
    Current.Job.on_cancel job (fun reason ->
        Logs.debug (fun l ->
            l "Calling the cluster on_cancel callback with reason: %s" reason);
        if reason <> "Job complete" then t.on_cancel reason;
        Lwt.return_unit)
    >>= fun () ->
    let { Value.base; variant; ty } = spec in
    let src = (Git.Commit_id.repo commit, [ Git.Commit_id.hash commit ]) in
    let cache_hint = get_cache_hint repo spec in
    let use_day10 =
      t.enable_day10 && day10_supported variant
      && match ty with `Opam (`Build, _, _) -> true | _ -> false
    in
    let action =
      match ty with
      | `Opam (`Build, selection, _) when use_day10 ->
          Current.Job.log job
            "Building with day10 (variant %a, opam-repository %s)" Variant.pp
            variant selection.Selection.commit;
          day10_action ~variant ~selection
            ~dune_args:[ "@install"; "@check"; "@runtest" ]
      | _ ->
          let build_spec = Build.make_build_spec ~base ~repo ~variant ~ty in
          Current.Job.write job
            (Fmt.str "@[<v>Base: %a@,%a@]@." Image.pp base Spec.pp_summary ty);
          Current.Job.write job
            (Fmt.str
               "@.To reproduce locally:@.@.%a@.cat > Dockerfile \
                <<'END-OF-DOCKERFILE'@.\o033[34m%s\o033[0m@.END-OF-DOCKERFILE@.docker \
                build .@.END-REPRO-BLOCK@.@."
               Current_git.Commit_id.pp_user_clone commit
               (Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false
                  ~os:`Unix build_spec));
          let spec_str = Fmt.to_to_string Obuilder_spec.pp build_spec in
          Current.Job.log job "Using OBuilder spec:@.%s@." spec_str;
          Cluster_api.Submission.obuilder_build spec_str
    in
    Current.Job.log job "Using cache hint %S" cache_hint;
    (* day10 jobs go to the dedicated "test" pool (served by carpenter); only
       that pool understands the "day10" custom job kind. Since [use_day10]
       implies the RISC-V variant, this just reroutes what would be
       linux-riscv64, leaving that pool untouched for other clients such as
       opam-repo-ci. *)
    let pool_name =
      if use_day10 then "test" else Platform.Pool_name.to_string pool
    in
    let build_pool =
      Current_ocluster.Connection.pool ~job ~pool:pool_name ~action ~cache_hint
        ~src t.connection
    in
    (* HACK: riscv and windows builders are slow;
       triple the per-job timeout there. *)
    let timeout =
      match (t.timeout, pool) with
      | Some t, `Windows_x86_64 | Some t, `Linux_riscv64 ->
          Some (Int64.mul t 3L)
      | timeout, _ -> timeout
    in
    Current.Job.start_with ~pool:build_pool job ?timeout
      ~level:Current.Level.Average
    >>= fun build_job ->
    Capability.with_ref build_job (Current_ocluster.Connection.run_job ~job)
    >>!= fun (_ : string) -> Lwt_result.return ()

  let pp f ({ Key.pool; repo; commit; label }, _) =
    Fmt.pf f "test %a %a (%s:%s)" Repo_id.pp repo Current_git.Commit_id.pp
      commit
      (Platform.Pool_name.to_string pool)
      label

  let auto_cancel = true
  let latched = true
end

module BC = Current_cache.Generic (Op)

let config ?timeout sr =
  let connection = Current_ocluster.Connection.create sr in
  let enable_day10 =
    match Sys.getenv_opt "OCAML_CI_USE_DAY10" with
    | Some ("1" | "true" | "yes" | "on") -> true
    | _ -> false
  in
  { connection; timeout; on_cancel = ignore; enable_day10 }

let build t ~platforms ~spec ~repo commit =
  Current.component "cluster build"
  |> let> { Spec.variant; ty; label } = spec
     and> commit
     and> platforms
     and> repo in
     match
       List.find_opt
         (fun p -> Variant.equal p.Platform.variant variant)
         platforms
     with
     | Some { Platform.builder = _; pool; variant; base; _ } ->
         BC.run t
           { Op.Key.pool; commit; repo; label }
           { Op.Value.base; ty; variant }
     | None ->
         (* We can only get here if there is a bug. If the set of platforms changes, [Analyse] should recalculate. *)
         let msg =
           Fmt.str "BUG: variant %a is not a supported platform" Variant.pp
             variant
         in
         Current_incr.const (Error (`Msg msg), None)

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with Some { Current.Metadata.job_id; _ } -> job_id | None -> None

let v t ?(on_cancel = ignore) ~platforms ~repo ~spec source =
  let t = { t with on_cancel } in
  let build = build t ~platforms ~spec ~repo source in
  let+ state = Current.state ~hidden:true build
  and+ job_id = get_job_id build
  and+ spec in
  let result =
    state
    |> Result.map @@ fun () ->
       match spec.ty with
       | `Opam_monorepo _ | `Opam (`Build, _, _) -> `Built
       | `Opam (`Lint (`Doc | `Opam), _, _) -> `Checked
       | `Opam_fmt _ -> `Checked
  in
  (result, job_id)
