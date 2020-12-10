(* PIPELINE
    1. Get all OS related variables for solver (OS-Vars)
    2. Solve for each locally without pulling images (Analysis)
    3. Generate workflow files (Workflows)
    4. Combine into one big file (Summary)
*)

open Current.Syntax 
open Ocaml_ci

module Git = Current_git
module Github = Current_github

module Ov = Ocaml_version
module OsVars = Ocaml_ci_api.Worker.Vars 

module GC = Current_cache.Generic(Op)

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with
  | Some { Current.Metadata.job_id; _ } -> job_id
  | None -> None

let local_builder = 
  let build_timeout = Duration.of_min 10 in 
  let pool = Current.Pool.create ~label:"github" 1 in 
  { Ocaml_ci.Builder.docker_context = None; pool; build_timeout }

let github_v ~vars ~repo ~spec source = 
  Current.component "write" |>
  let> { Spec.variant; ty; _ } = spec
  and> commit = source
  and> vars = vars
  and> repo = repo in
  match List.find_opt (fun v -> Variant.equal (fst v) variant) vars with 
  | Some (variant, var) ->
    let arch = Variant.arch variant 
    and distro = Variant.distro variant
    and ocaml_version = Variant.ocaml_version variant |> Ocaml_version.with_just_major_and_minor in begin
    match Variant.v ~arch ~distro ~ocaml_version with 
      | Ok variant -> 
        let ocaml_version = var.OsVars.ocaml_version in 
        let tag f = 
          let distro = Variant.distro f in 
          let version = var.OsVars.os_version in 
          let ocaml = Variant.ocaml_version f |> Ov.with_just_major_and_minor in 
        Fmt.strf "%s-%s-ocaml-%s" distro version (Ocaml_version.to_string ~sep:'-' ocaml
          |> String.map (function | '+' -> '-' | x -> x)) in 
        let base = "ocaml/opam:" ^ (tag variant) in 
        let github = begin
          if var.OsVars.os = "macos" then { Gh.mac = true; win = false; ovs = [ ocaml_version ] }
          else if var.os = "win32" then { Gh.win = true; mac = false; ovs = [ ocaml_version ] } 
          else { win = false; mac = false; ovs = [] } end in
        GC.run local_builder { Op.Key.commit; repo; label = base; github } { Op.Value.base; ty; variant }
      | Error (`Msg m) -> failwith m 
    end 
  | None ->
    let msg = Fmt.strf "BUG: variant %a is not a supported platform" Variant.pp variant in
    Current_incr.const (Error (`Msg msg), None)


let opam_repository_commit =
  let repo = { Github.Repo_id.owner = "ocaml"; name = "opam-repository" } in
  Github.Api.Anonymous.head_of repo @@ `Ref "refs/heads/master"

let vars =
  let variant {Ocaml_ci_api.Worker.Vars.os_distribution=distro; ocaml_version; arch; _ } = 
    match Variant.v ~arch:(Option.value ~default:`X86_64 @@ Ocaml_version.of_opam_arch arch) ~distro ~ocaml_version:(Ocaml_version.of_string_exn ocaml_version) with 
      | Ok v -> v 
      | Error (`Msg m) -> failwith m 
    in
  List.map (fun v -> variant v, v) (Os.vars (Ov.of_string_exn "4.11.1")) |> Current.return

let gen_workflows ~repo ~vars ~analysis source = 
  Current.with_context analysis @@ fun () ->
    let specs =
      let+ analysis = Current.state ~hidden:true analysis in
      match analysis with
      | Error _ ->
          (* If we don't have the analysis yet, just use the empty list. *)
          []
      | Ok analysis ->
        match Analyse.Analysis.selections analysis with
        | `Duniverse _ -> [] (* We'll just not support these for now... *)
        | `Opam_monorepo (_, _) -> []
        | `Opam_build selections ->
            selections |> List.map (fun selection ->
                let label = Variant.to_string selection.Selection.variant in
                Spec.opam ~label ~selection ~analysis `Build
              )
    in
    let builds = specs |> Current.list_map (module Spec) (fun spec ->
      let+ result =  
          let build = github_v ~vars ~repo ~spec source  in 
          let+ state = Current.state ~hidden:true build
          and+ job_id = get_job_id build
          and+ spec = spec
          and+ b = build in 
          let result =
            state |> Result.map @@ fun _ ->
            match spec.ty with
            | `Duniverse _ 
            | `Opam_monorepo _
            | `Opam (`Build, _, _) -> `Built
            | `Opam (`Lint (`Doc|`Opam), _, _) -> `Checked
            | `Opam_fmt _ -> `Checked
          in
          result, Some b, job_id
      and+ spec = spec in
      Spec.label spec, result
    ) in
    let+ builds = builds
    and+ analysis_result = Current.state ~hidden:true (Current.map (fun _ -> `Checked) analysis)
    and+ analysis_id = get_job_id analysis in
    builds @ [
      "(analysis)", (analysis_result, None, analysis_id)
    ]

let list_errors ~ok errs =
  let groups =  (* Group by error message *)
    List.sort compare errs |> List.fold_left (fun acc (msg, l) ->
        match acc with
        | (m2, ls) :: acc' when m2 = msg -> (m2, l :: ls) :: acc'
        | _ -> (msg, [l]) :: acc
      ) []
  in
  Error (`Msg (
      match groups with
      | [] -> "No builds at all!"
      | [ msg, _ ] when ok = 0 -> msg (* Everything failed with the same error *)
      | [ msg, ls ] -> Fmt.strf "%a failed: %s" Fmt.(list ~sep:(unit ", ") string) ls msg
      | _ ->
        (* Multiple error messages; just list everything that failed. *)
        let pp_label f (_, l) = Fmt.string f l in
        Fmt.strf "%a failed" Fmt.(list ~sep:(unit ", ") pp_label) errs
    ))

let summarise repo results =
  let open Workflow in 
  let dots_to_underscores s = String.(concat "_" (split_on_char '.' s)) in 
  let job_name f = 
    match Astring.String.cut ~sep:":" f.Op.Outcome.variant with 
      | Some (_, name) -> dots_to_underscores name
      | _ -> failwith "Unknown OS platform name"
  in
  let make_yaml (jobs : Op.Outcome.t list) : Yaml.value = 
    `O (List.map (fun (f : Op.Outcome.t) -> (job_name f, Types.job_to_yaml f.action.job)) jobs) 
  in  
  let jobs = results |> List.map (fun (_, job, _) -> match job with Some job -> [job] | None -> []) |> List.flatten in 
  let t = t (make_yaml jobs) |> with_name "Github OCaml-CI" |> with_on (simple_event ["push"; "pull_request"]) in 
  let s = Fmt.(str "%a" (Pp.workflow ~drop_null:true (fun a -> a)) t) in
  if List.length jobs > 0 then begin
    ignore (Bos.OS.Dir.create Fpath.(repo / ".github" / "workflows" ));
    (match Bos.OS.File.write Fpath.(repo / ".github" / "workflows" / "ocaml-ci.yml") s with 
    | Ok _ -> ()
    | Error (`Msg m) -> failwith m)
  end;
  results |> List.fold_left (fun (ok, pending, err, skip) -> function
      | _, _, Ok `Checked -> (ok, pending, err, skip)  (* Don't count lint checks *)
      | _, _, Ok `Built -> (ok + 1, pending, err, skip)
      | l, _, Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m -> (ok, pending, err, (m, l) :: skip)
      | l, _, Error `Msg m -> (ok, pending, (m, l) :: err, skip)
      | _, _, Error `Active _ -> (ok, pending + 1, err, skip)
    ) (0, 0, [], [])
  |> fun (ok, pending, err, skip) ->
  if pending > 0 then Error (`Active `Running)
  else match ok, err, skip with
    | 0, [], skip -> list_errors ~ok:0 skip (* Everything was skipped - treat skips as errors *)
    | _, [], _ -> Ok s                      (* No errors and at least one success *)
    | ok, err, _ -> list_errors ~ok err     (* Some errors found - report *)


let generate ~solver repository () = 
  let r = Git.Local.v repository in 
  let src = Git.Local.head_commit r in 
  let repo = Current.return { Github.Repo_id.owner = "local"; name = "test" } 
  and analysis = Analyse.examine_vars ~solver ~vars ~opam_repository_commit src in 
  Current.component "summarise" |>
  let> results = gen_workflows ~vars ~repo ~analysis src in
  let results =
    List.map (fun (variant, (build, job, _)) -> variant, job, build) results 
  in 
  let result = summarise repository results in 
  Current_incr.const (result, None)

