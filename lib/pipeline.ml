let list_errors ~ok errs =
  let groups =
    (* Group by error message *)
    List.sort compare errs
    |> List.fold_left
         (fun acc (msg, l) ->
           match acc with
           | (m2, ls) :: acc' when m2 = msg ->
               (m2, l.Build_info.label :: ls) :: acc'
           | _ -> (msg, [ l.Build_info.label ]) :: acc)
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
          let pp_label f (_, l) = Fmt.string f l.Build_info.label in
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
             (ok, pending, err, (m, l.Build_info.label) :: skip)
         | l, (Error (`Msg _ | `Active _), _)
           when Build_info.experimental_variant l ->
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
          (List.map (fun (m, l) -> (m, Build_info.of_label l)) skip)
        (* Everything was skipped - treat skips as errors *)
    | _, [], _ -> Ok () (* No errors and at least one success *)
    | ok, err, _ -> list_errors ~ok err (* Some errors found - report *)

open Current.Syntax
module Git = Current_git

let take_lowest_bound_selection = function
  | [] -> []
  | hd :: _ as selections ->
      List.fold_left
        (fun (v : Selection.t) (v' : Selection.t) ->
          if
            Ocaml_version.compare
              (Variant.ocaml_version v.variant)
              (Variant.ocaml_version v'.variant)
            <= 0
          then v
          else v')
        hd selections
      |> fun s -> [ s ]

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
            Spec.opam ~label:Variant.analysis_label ~selection:lint_selection
              ~analysis (`Lint `Fmt)
            :: Spec.opam_monorepo builds
        | `Opam_build selections ->
            let lint_selection =
              (* Sort by OCaml version and take the first Linux x86_64 selection. *)
              let sorted =
                List.sort
                  (fun x y ->
                    Ocaml_version.compare
                      (Variant.ocaml_version x.Selection.variant)
                      (Variant.ocaml_version y.Selection.variant))
                  selections
              in
              List.find
                (fun x ->
                  Variant.arch x.Selection.variant == `X86_64
                  && Variant.os x.Selection.variant == `linux)
                sorted
            in
            let lint_ocamlformat =
              match Analyse.Analysis.ocamlformat_selection analysis with
              | None -> lint_selection
              | Some selection -> selection
            in
            (* For lower-bound, take only the lowest version of OCaml that has a solution *)
            let selections =
              let lower_bound, other =
                List.partition
                  (fun s ->
                    s.Selection.lower_bound
                    && Variant.arch s.Selection.variant == `X86_64
                    && Variant.os s.Selection.variant == `linux)
                  selections
              in
              take_lowest_bound_selection lower_bound @ other
            in
            let builds s =
              Selection.filter_duplicate_opam_versions s
              |> List.map (fun selection ->
                     let label =
                       if selection.Selection.lower_bound then
                         Variant.lower_bound_label
                       else Variant.to_string selection.Selection.variant
                     in
                     Spec.opam ~label ~selection ~analysis `Build)
            and lint =
              [
                Spec.opam ~label:Variant.fmt_label ~selection:lint_ocamlformat
                  ~analysis (`Lint `Fmt);
                Spec.opam ~label:Variant.doc_label ~selection:lint_selection
                  ~analysis (`Lint `Doc);
                Spec.opam ~label:Variant.opam_label ~selection:lint_selection
                  ~analysis (`Lint `Opam);
              ]
            in
            lint @ builds selections)
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
           (Build_info.of_spec spec, result))
  in
  let+ builds
  and+ analysis_result =
    Current.state ~hidden:true (Current.map (fun _ -> `Checked) analysis)
  and+ analysis_id = get_job_id analysis in
  builds
  @ [
      ( Build_info.of_label Variant.analysis_label,
        (analysis_result, analysis_id) );
    ]
