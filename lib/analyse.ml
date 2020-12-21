open Lwt.Infix
open Current.Syntax

module Worker = Ocaml_ci_api.Worker

let pool = Current.Pool.create ~label:"analyse" 2

let is_empty_file x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_REG; st_size = 0; _ } -> true
  | _ -> false

let is_toplevel path = not (String.contains path '/')

let ( >>!= ) = Lwt_result.bind

let read_file ~max_len path =
  Lwt_io.with_file ~mode:Lwt_io.input path
    (fun ch ->
       Lwt_io.length ch >>= fun len ->
       let len =
         if len <= Int64.of_int max_len then Int64.to_int len
         else Fmt.failwith "File %S too big (%Ld bytes)" path len
       in
       let buf = Bytes.create len in
       Lwt_io.read_into_exactly ch buf 0 len >|= fun () ->
       Bytes.to_string buf
    )

(* A logging service that logs to [job]. *)
let job_log job =
  let module X = Ocaml_ci_api.Raw.Service.Log in
  X.local @@ object
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
    ocamlformat_source : Analyse_ocamlformat.source option;
    selections :
      [ `Opam_build of Selection.t list
      | `Opam_monorepo of Opam_monorepo.config
      ];
  }
  [@@deriving yojson]

  let marshal t = to_yojson t |> Yojson.Safe.to_string

  let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with
    | Ok x -> x
    | Error e -> failwith e

  let opam_files t = t.opam_files

  let ocamlformat_source t = t.ocamlformat_source

  let selections t = t.selections

  let is_test_dir = Astring.String.is_prefix ~affix:"test"

  let check_opam_version =
    let version_2 = OpamVersion.of_string "2" in
    fun name opam ->
      let opam_version = OpamFile.OPAM.opam_version opam in
      if OpamVersion.compare opam_version version_2 < 0 then
        Fmt.failwith "Package %S uses unsupported opam version %s (need >= 2)" name (OpamVersion.to_string opam_version)

  (* For each package in [root_pkgs], parse the opam file and check whether it uses pin-depends.
     Fetch and return all pinned opam files. Also, ensure we're using opam format version 2. *)
  let handle_opam_files ~job ~root_pkgs ~pinned_pkgs =
    pinned_pkgs |> List.iter (fun (name, contents) ->
        check_opam_version name (OpamFile.OPAM.read_from_string contents)
      );
    let pin_depends =
      root_pkgs
      |> List.map (fun (name, contents) ->
          let opam =
            try
              OpamFile.OPAM.read_from_string contents
            with ex ->
              Fmt.failwith "Invalid opam file %S: %a" name Fmt.exn ex
          in
          check_opam_version name opam;
          let pin_depends = OpamFile.OPAM.pin_depends opam in
          pin_depends |> List.map (fun (pkg, url) ->
              Current.Job.log job "%s: found pin-depends: %s -> %s"
                name (OpamPackage.to_string pkg) (OpamUrl.to_string url);
              (name, pkg, url)
            )
        )
      |> List.concat
    in
    pin_depends |> Lwt_list.map_s (fun (root_pkg, pkg, url) ->
        Lwt.catch
          (fun () ->
             Pin_depends.get_opam ~job ~pkg url >|= fun contents ->
             (OpamPackage.to_string pkg, contents)
          )
          (function
            | Failure msg -> Fmt.failwith "%s (processing pin-depends in %s)" msg root_pkg
            | ex -> Lwt.fail ex
          )
      )

  let opam_selections ~solve ~job ~platforms ~opam_files dir =
    let src = Fpath.to_string dir in
    let ( / ) = Filename.concat in
    begin
      opam_files |> Lwt_list.fold_left_s (fun (root_pkgs, pinned_pkgs) path ->
          let name = Filename.basename path |> Filename.chop_extension in
          let name = if String.contains name '.' then name else name ^ ".dev" in
          read_file ~max_len:102400 (src / path) >|= fun file ->
          let item = name, file in
          if Filename.dirname path = "." then
            (item :: root_pkgs, pinned_pkgs)
          else
            (root_pkgs, item :: pinned_pkgs)
        ) ([], [])
    end >>= fun (root_pkgs, pinned_pkgs) ->
    Lwt.try_bind
      (fun () -> handle_opam_files ~job ~root_pkgs ~pinned_pkgs)
      (fun pin_depends ->
         let pinned_pkgs = pin_depends @ pinned_pkgs in
         Lwt_result.map
            (fun selections -> `Opam_build selections)
            (solve ~root_pkgs ~pinned_pkgs ~platforms)
      )
      (function
        | Failure msg -> Lwt_result.fail (`Msg msg)
        | ex -> Lwt.fail ex
      )

  let type_of_dir dir =
    match Opam_monorepo.detect ~dir with
    | Some info -> `Opam_monorepo info
    | None -> `Ocaml_repo

  (** Call the solver with a request containing these packages.
      When it returns a list, it is nonempty. *)
  let solve ~root_pkgs ~pinned_pkgs ~platforms ~opam_repository_commit ~job ~solver =
    let platforms = List.map (fun (variant,vars) -> Variant.to_string variant, vars) platforms in
    let request =
      { Ocaml_ci_api.Worker.Solve_request.
        opam_repository_commit = Current_git.Commit_id.hash opam_repository_commit;
        root_pkgs;
        pinned_pkgs;
        platforms
      }
    in
    Capnp_rpc_lwt.Capability.with_ref (job_log job) @@ fun log ->
    Ocaml_ci_api.Solver.solve solver request ~log >|= function
    | Ok [] -> Fmt.error_msg "No solution found for any supported platform"
    | Ok x -> Ok (List.map Selection.of_worker x)
    | Error (`Msg msg) -> Fmt.error_msg "Error from solver: %s" msg

  let of_dir ~solver ~job ~platforms ~opam_repository_commit dir =
    let solve = solve ~opam_repository_commit ~job ~solver in
    let ty = type_of_dir dir in
    let cmd = "", [| "find"; "."; "-maxdepth"; "3"; "-name"; "*.opam" |] in
    Current.Process.check_output ~cwd:dir ~cancellable:true ~job cmd >>!= fun output ->
    let opam_files =
      String.split_on_char '\n' output
      |> List.sort String.compare
      |> List.filter_map (function
          | "" -> None
          | path ->
            let path =
              if Astring.String.is_prefix ~affix:"./" path then
                Astring.String.with_range ~first:2 path
              else path
            in
            let consider_opam_file path =
              match Fpath.v path |> Fpath.segs with
              | [_file] -> true
              | segs when List.exists is_test_dir segs ->
                Current.Job.log job "Ignoring test directory %S" path;
                false
              | _ -> true
            in
            let full_path = Filename.concat (Fpath.to_string dir) path in
            if is_empty_file full_path then (
              Current.Job.log job "WARNING: ignoring empty opam file %S" path;
              None
            ) else if consider_opam_file path then
              Some path
            else None
        )
    in
    Analyse_ocamlformat.get_ocamlformat_source job ~opam_files ~root:dir >>!= fun ocamlformat_source ->
    if opam_files = [] then Lwt_result.fail (`Msg "No opam files found!")
    else if List.filter is_toplevel opam_files = [] then Lwt_result.fail (`Msg "No top-level opam files found!")
    else (
      begin
        match ty with
        | `Opam_monorepo info -> Opam_monorepo.selection ~info ~solve ~platforms
        | `Ocaml_repo -> opam_selections ~solve ~job ~platforms ~opam_files dir
      end >>!= fun selections ->
      let r = { opam_files; ocamlformat_source; selections } in
      Current.Job.log job "@[<v2>Results:@,%a@]" Yojson.Safe.(pretty_print ~std:true) (to_yojson r);
      Lwt_result.return r
    )
end

module Examine = struct
  type t = Ocaml_ci_api.Solver.t

  module Key = struct
    type t = Current_git.Commit.t

    let digest src =
      let json = `Assoc [
          "src", `String (Current_git.Commit.hash src);
        ]
      in
      Yojson.Safe.to_string json
  end

  module Value = struct
    type t = {
      opam_repository_commit : Current_git.Commit_id.t;
      platforms : (Variant.t * Worker.Vars.t) list;
    }

    let platform_to_yojson (variant, vars) =
      `Assoc [
        "variant", Variant.to_yojson variant;
        "vars", Worker.Vars.to_yojson vars
      ]

    let digest { opam_repository_commit; platforms } =
      let json = `Assoc [
          "opam-repository", `String (Current_git.Commit_id.hash opam_repository_commit);
          "platforms", `List (List.map platform_to_yojson platforms);
        ]
      in
      Yojson.Safe.to_string json
  end

  module Outcome = Analysis

  let id = "ci-analyse"

  let run solver job src { Value.opam_repository_commit; platforms } =
    Current.Job.start job ~pool ~level:Current.Level.Harmless >>= fun () ->
    Current_git.with_checkout ~job src @@ fun src ->
    Analysis.of_dir ~solver ~platforms ~opam_repository_commit ~job src

  let pp f _ = Fmt.string f "Analyse"

  let auto_cancel = true
  let latched = true
end

module Examine_cache = Current_cache.Generic(Examine)

let examine ~solver ~platforms ~opam_repository_commit src =
  Current.component "Analyse" |>
  let> src = src
  and> opam_repository_commit = opam_repository_commit
  and> platforms = platforms in
  let platforms = platforms |> List.map (fun { Platform.variant; vars; _ } -> (variant, vars)) in
  Examine_cache.run solver src { Examine.Value.opam_repository_commit; platforms }
