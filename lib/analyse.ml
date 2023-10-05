open Lwt.Infix
open Current.Syntax
module Worker = Ocaml_ci_api.Worker
module Content = Repo_content.Content

let pool = Current.Pool.create ~label:"analyse" 150
let ( >>!= ) = Lwt_result.bind

(* A logging service that logs to [job]. *)
let job_log job =
  let module X = Ocaml_ci_api.Raw.Solve.Service.Log in
  X.local
  @@ object
       inherit X.service

       method write_impl params release_param_caps =
         let open X.Write in
         release_param_caps ();
         let msg = Params.msg_get params in
         Current.Job.write job msg;
         Capnp_rpc_lwt.Service.(return (Response.create_empty ()))
     end

module Analysis = struct
  type t = {
    opam_files : string list;
    ocamlformat_selection : Selection.t option;
    ocamlformat_source : Analyse_ocamlformat.source option;
    selections :
      [ `Opam_build of Selection.t list
      | `Opam_monorepo of Opam_monorepo.config list ];
  }
  [@@deriving yojson]

  let marshal t = to_yojson t |> Yojson.Safe.to_string

  let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with
    | Ok x -> x
    | Error e -> failwith e

  let opam_files t = t.opam_files
  let ocamlformat_selection t = t.ocamlformat_selection
  let ocamlformat_source t = t.ocamlformat_source
  let selections t = t.selections

  (** Call the solver with a request containing these packages. When it returns
      a list, it is nonempty. *)
  let solve ~root_pkgs ~pinned_pkgs ~platforms ~opam_repository_commit ~job
      ~solver =
    let platforms =
      List.map
        (fun (variant, vars) -> (Variant.to_string variant, vars))
        platforms
    in
    let request =
      {
        Ocaml_ci_api.Worker.Solve_request.opam_repository_commits =
          [
            ( Current_git.Commit_id.repo opam_repository_commit,
              Current_git.Commit_id.hash opam_repository_commit );
          ];
        root_pkgs;
        pinned_pkgs;
        platforms;
        (* TODO ocamlformat in the request (option)*)
        (* TODO opam_monorepo in the request (option)*)
      }
    in
    Current.Job.log job "Solving with opam-repository commit: %a"
      Current_git.Commit_id.pp opam_repository_commit;
    Capnp_rpc_lwt.Capability.with_ref (job_log job) @@ fun log ->
    Backend_solver.solve solver job request ~log >|= function
    | Error `Cancelled -> Fmt.error_msg "Job cancelled"
    | Error (`Msg msg) -> Fmt.error_msg "Error from solver: %s" msg
    | Ok [] -> Fmt.error_msg "No solution found for any supported platform"
    | Ok x -> (
        let has_no_platform (pkg_version, _) =
          if
            List.exists
              (fun (sel : Worker.Selection.t) ->
                List.mem pkg_version sel.compat_pkgs)
              x
          then None
          else Some pkg_version
        in
        match List.filter_map has_no_platform root_pkgs with
        | [] ->
            let root_pkgs = List.map fst root_pkgs in
            Ok (List.map (Selection.of_worker ~root_pkgs) x)
        | missing_pkgs ->
            Fmt.error_msg "No solution found for %a on any supported platform"
              Fmt.(list ~sep:comma Dump.string)
              missing_pkgs)

  let rec lwt_result_list_mapm ~f = function
    | [] -> Lwt_result.return []
    | x :: xs ->
        let open Lwt_result.Syntax in
        let+ y = f x and+ ys = lwt_result_list_mapm ~f xs in
        y :: ys

  let opam_dep_file packages =
    {|opam-version: "2.0"|}
    :: {|depends: [|}
    :: List.map
         (fun (pkg, ver) -> Printf.sprintf {|  "%s" %s|} pkg ver)
         packages
    @ [ "]\n" ]
    |> String.concat "\n"

  let exactly v = Printf.sprintf {|{ = "%s" }|} v
  let solver_cache = Hashtbl.create 128

  let find_opam_repo_commit_for_ocamlformat ~solve ~platforms version =
    let ( let+ ) = Lwt_result.Infix.( >|= ) in
    let deps_for_ocamlformat =
      ( "deps_for_ocamlformat.opam",
        opam_dep_file [ ("ocamlformat", exactly version) ] )
    in
    let+ selections =
      match Hashtbl.find_opt solver_cache version with
      | Some (selections, platforms') ->
          if platforms <> platforms' then (
            let+ selections =
              solve ~root_pkgs:[ deps_for_ocamlformat ] ~pinned_pkgs:[]
                ~platforms
            in
            Hashtbl.add solver_cache version (selections, platforms);
            selections)
          else Lwt_result.return selections
      | None ->
          let+ selections =
            solve ~root_pkgs:[ deps_for_ocamlformat ] ~pinned_pkgs:[] ~platforms
          in
          Hashtbl.add solver_cache version (selections, platforms);
          selections
    in
    let selection = List.hd selections in
    (selection.Selection.commit, selection)

  let filter_linux_x86_64_platforms platforms =
    List.filter
      (fun (v, _) -> Variant.arch v == `X86_64 && Variant.os v == `linux)
      platforms
    |> List.sort (fun (v0, _) (v1, _) ->
           Ocaml_version.compare (Variant.ocaml_version v0)
             (Variant.ocaml_version v1))

  let of_content ~solver ~job ~platforms ~opam_repository_commit src =
    let opam_files = Content.opam_files src in
    let version = Content.ocamlformat_version src in
    let solve = solve ~opam_repository_commit ~job ~solver in
    let find_opam_repo_commit =
      find_opam_repo_commit_for_ocamlformat ~solve
        ~platforms:(filter_linux_x86_64_platforms platforms)
    in
    Analyse_ocamlformat.get_ocamlformat_source job ~opam_files ~version
      ~find_opam_repo_commit
    >>!= fun (ocamlformat_source, ocamlformat_selection) ->
    if opam_files = [] then Lwt_result.fail (`Msg "No opam files found!")
    else if List.filter Fpath.is_seg opam_files = [] then
      Lwt_result.fail (`Msg "No top-level opam files found!")
    else
      (match Content.dir_type src with
      | `Opam_monorepo builds ->
          lwt_result_list_mapm builds ~f:(fun info ->
              Opam_monorepo.selection ~info ~solve ~platforms)
          |> Lwt_result.map (fun l -> `Opam_monorepo l)
      | `Ocaml_repo ->
          let root_pkgs = Content.root_pkgs src in
          let pinned_pkgs = Content.pinned_pkgs src in
          Lwt_result.map
            (fun selections -> `Opam_build selections)
            (solve ~root_pkgs ~pinned_pkgs ~platforms))
      >>!= fun selections ->
      let r =
        { opam_files; ocamlformat_selection; ocamlformat_source; selections }
      in
      Current.Job.log job "@[<v2>Results:@,%a@]"
        Yojson.Safe.(pretty_print ~std:true)
        (to_yojson r);
      Lwt_result.return r
end

module Examine = struct
  type t = Backend_solver.t

  module Key = struct
    type t = Current_git.Commit.t

    let digest src =
      let json = `Assoc [ ("src", `String (Current_git.Commit.hash src)) ] in
      Yojson.Safe.to_string json
  end

  module Value = struct
    type t = {
      opam_repository_commit : Current_git.Commit_id.t;
      platforms : (Variant.t * Worker.Vars.t) list;
      src_content : Repo_content.Content.t;
    }

    let platform_to_yojson (variant, vars) =
      `Assoc
        [
          ("variant", Variant.to_yojson variant);
          ("vars", Worker.Vars.to_yojson vars);
        ]

    let digest { opam_repository_commit; platforms; src_content } =
      let json =
        `Assoc
          [
            ( "opam-repository",
              `String (Current_git.Commit_id.hash opam_repository_commit) );
            ("platforms", `List (List.map platform_to_yojson platforms));
            ("src_content", `String (Repo_content.Content.marshal src_content));
          ]
      in
      Yojson.Safe.to_string json
  end

  module Outcome = Analysis

  let id = "ci-analyse"

  let run solver job _ { Value.opam_repository_commit; platforms; src_content }
      =
    Current.Job.start job ~pool ~level:Current.Level.Harmless >>= fun () ->
    Analysis.of_content ~solver ~platforms ~opam_repository_commit ~job
      src_content

  let pp f _ = Fmt.string f "Analyse"
  let auto_cancel = true
  let latched = true
end

module Examine_cache = Current_cache.Generic (Examine)

let examine ~solver ~platforms ~opam_repository_commit src src_content =
  Current.component "Analyse"
  |> let> src and> opam_repository_commit and> platforms and> src_content in
     let platforms =
       platforms
       |> List.map (fun { Platform.variant; vars; _ } -> (variant, vars))
     in
     Examine_cache.run solver src
       { Examine.Value.opam_repository_commit; platforms; src_content }
