type build_info = { label : string; variant : Variant.t option }

let build_info_of_spec = function
  | Spec.{ label; variant; ty = _ } -> { label; variant = Some variant }

let build_info_of_label label = { label; variant = None }

let experimental_variant s =
  if Astring.String.(
    is_prefix ~affix:"(lint-lower-bounds)" s.label
    || is_prefix ~affix:"(lint-opam)" s.label) then true
  else
    match s.variant with
    | None -> false
    | Some v ->
        Astring.String.equal "macos-homebrew" (Variant.distro v)
        || Ocaml_version.(equal (v 5 1 ~patch:0)) (Variant.ocaml_version v)
        || Ocaml_version.(equal (v 5 1 ~patch:0 ~prerelease:"alpha1"))
             (Variant.ocaml_version v)

let list_errors ~ok errs =
  let groups =
    (* Group by error message *)
    List.sort compare errs
    |> List.fold_left
         (fun acc (msg, l) ->
           match acc with
           | (m2, ls) :: acc' when m2 = msg -> (m2, l.label :: ls) :: acc'
           | _ -> (msg, [ l.label ]) :: acc)
         []
  in
  Error
    (`Msg
      (match groups with
      | [] -> "No builds at all!"
      | [ (msg, _) ] when ok = 0 ->
          msg (* Everything failed with the same error *)
      | [ (msg, ls) ] ->
          Fmt.str "%a failed: %s" Fmt.(list ~sep:(any ", ") string) ls msg
      | _ ->
          (* Multiple error messages; just list everything that failed. *)
          let pp_label f (_, l) = Fmt.string f l.label in
          Fmt.str "%a failed" Fmt.(list ~sep:(any ", ") pp_label) errs))

let summarise results =
  results
  |> List.fold_left
       (fun (ok, pending, err, skip) -> function
         | _, (Ok `Checked, _) ->
             (ok, pending, err, skip) (* Don't count lint checks *)
         | _, (Ok `Built, _) -> (ok + 1, pending, err, skip)
         | l, (Error (`Msg m), _)
           when Astring.String.is_prefix ~affix:"[SKIP]" m ->
             (ok, pending, err, (m, l.label) :: skip)
         | l, (Error (`Msg _ | `Active _), _) when experimental_variant l ->
             (ok + 1, pending, err, skip)
         (* Don't fail the commit if an experimental build failed. *)
         | l, (Error (`Msg m), _) -> (ok, pending, (m, l) :: err, skip)
         | _, (Error (`Active _), _) -> (ok, pending + 1, err, skip))
       (0, 0, [], [])
  |> fun (ok, pending, err, skip) ->
  if pending > 0 then Error (`Active `Running)
  else
    match (ok, err, skip) with
    | 0, [], skip ->
        list_errors ~ok:0
          (List.map (fun (m, l) -> (m, build_info_of_label l)) skip)
        (* Everything was skipped - treat skips as errors *)
    | _, [], _ -> Ok () (* No errors and at least one success *)
    | ok, err, _ -> list_errors ~ok err (* Some errors found - report *)

open Current.Syntax
module Git = Current_git

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with Some { Current.Metadata.job_id; _ } -> job_id | None -> None

let build_with_docker ?ocluster ?on_cancel ~(repo : Repo_id.t Current.t)
    ~analysis ~platforms source =
  Current.with_context analysis @@ fun () ->
  let specs =
    let+ analysis = Current.state ~hidden:true analysis in
    match analysis with
    | Error _ ->
        (* If we don't have the analysis yet, just use the empty list. *)
        []
    | Ok analysis -> (
        match Analyse.Analysis.selections analysis with
        | `Opam_monorepo builds ->
            let lint_selection =
              Opam_monorepo.selection_of_config (List.hd builds)
            in
            Spec.opam ~label:"(lint-fmt)" ~selection:lint_selection ~analysis
              (`Lint `Fmt)
            :: Spec.opam_monorepo builds
        | `Opam_build (`Default selections, `Lower_bound lower_bound_selections)
          ->
            let lint_selection = List.hd selections in
            let lint_ocamlformat =
              match Analyse.Analysis.ocamlformat_selection analysis with
              | None -> lint_selection
              | Some selection -> selection
            in
            let builds ~is_lower_bound s =
              Selection.filter_duplicate_opam_versions s
              |> List.map (fun selection ->
                     let label =
                       if is_lower_bound then "(lint-lower-bounds)"
                       else Variant.to_string selection.Selection.variant
                     in
                     Spec.opam ~label ~selection ~analysis `Build)
            and lint =
              [
                Spec.opam ~label:"(lint-fmt)" ~selection:lint_ocamlformat
                  ~analysis (`Lint `Fmt);
                Spec.opam ~label:"(lint-doc)" ~selection:lint_selection
                  ~analysis (`Lint `Doc);
                Spec.opam ~label:"(lint-opam)" ~selection:lint_selection
                  ~analysis (`Lint `Opam);
              ]
            in
            lint
            @ builds ~is_lower_bound:false selections
            @ builds ~is_lower_bound:true lower_bound_selections)
  in
  let builds =
    specs
    |> Current.list_map
         (module Spec)
         (fun spec ->
           let+ result =
             match ocluster with
             | None -> Build.v ~platforms ~repo ~spec source
             | Some ocluster ->
                 let src = Current.map Git.Commit.id source in
                 Cluster_build.v ocluster ?on_cancel ~platforms ~repo ~spec src
           and+ spec in
           (build_info_of_spec spec, result))
  in
  let+ builds
  and+ analysis_result =
    Current.state ~hidden:true (Current.map (fun _ -> `Checked) analysis)
  and+ analysis_id = get_job_id analysis in
  builds
  @ [ (build_info_of_label "(analysis)", (analysis_result, analysis_id)) ]
