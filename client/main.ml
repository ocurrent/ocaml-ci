open Astring
open Lwt.Infix
open Capnp_rpc_lwt
module Client = Ocaml_ci_api.Client

let errorf msg = msg |> Fmt.kstr @@ fun msg -> Error (`Msg msg)

let with_ref r fn =
  Lwt.finalize
    (fun () -> fn r)
    (fun () ->
      Capability.dec_ref r;
      Lwt.return_unit)

let show_log job =
  let rec aux start =
    Current_rpc.Job.log ~start job >>= function
    | Error _ as e -> Lwt.return e
    | Ok (data, next) ->
        if data = "" then Lwt_result.return ()
        else (
          output_string stdout data;
          flush stdout;
          aux next)
  in
  aux 0L

let show_status job =
  Current_rpc.Job.status job
  |> Lwt_result.map
     @@ fun { Current_rpc.Job.id; description; can_cancel; can_rebuild } ->
     Fmt.pr
       "@[<v2>Job %S:@,Description: @[%a@]@,Can cancel: %b@,Can rebuild: %b@]@."
       id Fmt.lines description can_cancel can_rebuild

let cancel job =
  Current_rpc.Job.cancel job |> Lwt_result.map @@ fun () -> Fmt.pr "Cancelled@."

let rebuild job =
  Fmt.pr "Requesting rebuild...@.";
  with_ref (Current_rpc.Job.rebuild job) show_log

let import_ci_ref ~vat = function
  | Some url -> Capnp_rpc_unix.Vat.import vat url
  | None -> (
      match Sys.getenv_opt "HOME" with
      | None -> errorf "$HOME not set! Can't get default cap file location.@."
      | Some home ->
          let path = Filename.concat home ".ocaml-ci.cap" in
          if Sys.file_exists path then Capnp_rpc_unix.Cap_file.load vat path
          else errorf "Default cap file %S not found!" path)

let list_orgs ci =
  Client.CI.orgs ci
  |> Lwt_result.map @@ function
     | [] ->
         Fmt.pr
           "@[<v>No owner (organisation) given and no suggestions available."
     | orgs ->
         Fmt.pr
           "@[<v>No owner (organisation) given. Try one of these:@,@,%a@]@."
           Fmt.(list string)
           orgs

let list_repos ~owner org =
  Client.Org.repos org
  |> Lwt_result.map @@ function
     | [] -> Fmt.pr "@[<v>No repository given and no suggestions available.@."
     | repos ->
         let full_name f { Client.Org.name; main_status; _ } =
           Fmt.pf f "%s/%s (%a)" owner name Client.Build_status.pp main_status
         in
         Fmt.pr "@[<v>No repository given. Try one of these:@,@,%a@]@."
           Fmt.(list full_name)
           repos

let list_refs repo =
  Client.Repo.refs repo
  |> Lwt_result.map @@ fun refs ->
     if Client.Ref_map.is_empty refs then
       Fmt.pr
         "No branches or PRs are being tracked by the CI for this repository.@."
     else
       Client.Ref_map.iter
         (fun gref { Client.Repo.hash; status; _ } ->
           Fmt.pr "%s %s (%a)@." hash gref Client.Build_status.pp status)
         refs

let pp_timestamp f v =
  Fmt.pf f "%a"
    Fmt.(option ~none:(any "-") (Timedesc.pp_iso8601 ()))
    (Option.bind v Timedesc.of_timestamp_float_s)

let pp_job f
    {
      Client.variant;
      outcome;
      queued_at;
      started_at;
      finished_at;
      is_experimental;
    } =
  let experimental = if is_experimental then " (experimental)" else "" in
  Fmt.pf f "%s (%a) (Queued_at: %a) (Started_at: %a) (Finished_at: %a)%s"
    variant Client.State.pp outcome pp_timestamp queued_at pp_timestamp
    started_at pp_timestamp finished_at experimental

let list_variants commit =
  Client.Commit.jobs commit
  |> Lwt_result.map @@ function
     | [] -> Fmt.pr "No jobs found for this commit!@."
     | jobs -> Fmt.pr "@[<v>%a@]@." Fmt.(list pp_job) jobs

let main ~ci_uri ~repo ~target ~variant ~job_op =
  let vat = Capnp_rpc_unix.client_only_vat () in
  match import_ci_ref ~vat ci_uri with
  | Error _ as e -> Lwt.return e
  | Ok sr -> (
      Sturdy_ref.connect_exn sr >>= fun ci ->
      match repo with
      | None -> list_orgs ci
      | Some repo -> (
          match String.cut ~sep:"/" repo with
          | None ->
              let owner = repo in
              with_ref (Client.CI.org ci owner) (list_repos ~owner)
          | Some (owner, name) -> (
              with_ref (Client.CI.org ci owner) @@ fun org ->
              with_ref (Client.Org.repo org name) @@ fun repo ->
              match target with
              | None -> list_refs repo
              | Some r -> (
                  let commit =
                    match r with
                    | `Ref target -> Client.Repo.commit_of_ref repo target
                    | `Commit hash -> Client.Repo.commit_of_hash repo hash
                  in
                  with_ref commit @@ fun commit ->
                  match variant with
                  | None -> list_variants commit
                  | Some variant ->
                      with_ref
                        (Client.Commit.job_of_variant commit variant)
                        job_op))))

(* Command-line parsing *)

open Cmdliner

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(
    const Logging.init
    $ Fmt_cli.style_renderer ~docs ()
    $ Logs_cli.level ~docs ())

let cap =
  Arg.value
  @@ Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info ~doc:"The ocaml-ci.cap file." ~docv:"CAP" [ "ci-cap" ]

let repo =
  Arg.value
  @@ Arg.pos 0 Arg.(some string) None
  @@ Arg.info ~doc:"The GitHub repository to use (org/name)." ~docv:"REPO" []

let gref =
  let make_ref s =
    if String.is_prefix ~affix:"refs/pull/" s then
      match String.cuts ~sep:"/" s with
      | [ "refs"; "pull"; pr ] -> Ok (`Ref (Fmt.str "refs/pull/%s/head" pr))
      | _ -> Ok (`Ref s)
    else Ok (`Ref s)
  in
  let parse s =
    if not (Stdlib.String.contains s '/') then
      if String.length s < 6 then
        Error
          (`Msg
             "Git reference should start 'refs/' or be a hash at least 6 \
              characters long")
      else Ok (`Commit s)
    else if String.is_prefix ~affix:"refs/" s then make_ref s
    else make_ref ("refs/" ^ s)
  in
  let pp f = function
    | `Commit s -> Fmt.string f s
    | `Ref r -> Fmt.string f r
  in
  Arg.conv (parse, pp)

let target =
  Arg.value
  @@ Arg.pos 1 Arg.(some gref) None
  @@ Arg.info
       ~doc:
         "The branch, commit or pull request to use. e.g. heads/master or \
          pull/3"
       ~docv:"TARGET" []

let variant =
  Arg.value
  @@ Arg.pos 2 Arg.(some string) None
  @@ Arg.info ~doc:"The build matrix variant" ~docv:"VARIANT" []

let job_op =
  let ops =
    [
      ("log", `Show_log);
      ("status", `Show_status);
      ("cancel", `Cancel);
      ("rebuild", `Rebuild);
    ]
  in
  Arg.value
  @@ Arg.pos 3 Arg.(enum ops) `Show_status
  @@ Arg.info ~doc:"The operation to perform (log, status, cancel or rebuild)."
       ~docv:"METHOD" []

(* (cmdliner's [enum] can't cope with functions) *)
let to_fn = function
  | `Cancel -> cancel
  | `Rebuild -> rebuild
  | `Show_log -> show_log
  | `Show_status -> show_status

let cmd =
  let doc = "Client for ocaml-ci" in
  let main () ci_uri repo target variant job_op =
    let job_op = to_fn job_op in
    match Lwt_main.run (main ~ci_uri ~repo ~target ~variant ~job_op) with
    | Ok () -> ()
    | Error (`Capnp ex) ->
        Fmt.epr "%a@." Capnp_rpc.Error.pp ex;
        exit 1
    | Error (`Msg m) ->
        Fmt.epr "%s@." m;
        exit 1
  in
  let info = Cmd.info "ocaml-ci" ~doc in
  Cmd.v info
    Term.(const main $ setup_log $ cap $ repo $ target $ variant $ job_op)

let () = exit @@ Cmd.eval cmd
