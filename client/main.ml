open Astring
open Lwt.Infix
open Capnp_rpc_lwt

module Client = Ocaml_ci_api.Client

let () =
  Logging.init ~level:Logs.Warning ()

let errorf msg =
  msg |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

let with_ref r fn =
  Lwt.finalize
    (fun () -> fn r)
    (fun () -> Capability.dec_ref r; Lwt.return_unit)

let show_log job =
  let rec aux start =
    Current_rpc.Job.log ~start job >>= function
    | Error _ as e -> Lwt.return e
    | Ok (data, next) ->
      if data = "" then Lwt_result.return ()
      else (
        output_string stdout data;
        flush stdout;
        aux next
      )
  in
  aux 0L

let show_status job =
  Current_rpc.Job.status job |> Lwt_result.map @@
  fun { Current_rpc.Job.id; description; can_cancel; can_rebuild } ->
  Fmt.pr "@[<v2>Job %S:@,\
          Description: @[%a@]@,\
          Can cancel: %b@,\
          Can rebuild: %b@]@."
    id
    Fmt.lines description
    can_cancel
    can_rebuild

let cancel job =
  Current_rpc.Job.cancel job |> Lwt_result.map @@ fun () ->
  Fmt.pr "Cancelled@."

let rebuild job =
  Fmt.pr "Requesting rebuild...@.";
  with_ref (Current_rpc.Job.rebuild job) show_log

let import_ci_ref ~vat = function
  | Some url -> Capnp_rpc_unix.Vat.import vat url
  | None ->
    match Sys.getenv_opt "HOME" with
    | None -> errorf "$HOME not set! Can't get default cap file location."
    | Some home ->
      let path = Filename.concat home ".ocaml-ci.cap" in
      if Sys.file_exists path then
        Capnp_rpc_unix.Cap_file.load vat path
      else
        errorf "Default cap file %S not found!" path

let main ~ci_uri ~repo ~target ~job_op =
  let vat = Capnp_rpc_unix.client_only_vat () in
  match import_ci_ref ~vat ci_uri with
  | Error _ as e -> Lwt.return e
  | Ok sr ->
    Sturdy_ref.connect_exn sr >>= fun ci ->
    match String.cut ~sep:"/" repo with
    | None ->
      Lwt_result.fail (`Msg (Fmt.strf "Repo should be in the form owner/name, not %S" repo))
    | Some (owner, name) ->
      with_ref (Client.CI.org ci owner) @@ fun org ->
      with_ref (Client.Org.repo org name) @@ fun repo ->
      match target with
      | None ->
        Client.Repo.refs repo |> Lwt_result.map @@ fun refs ->
        Client.Ref_map.iter (fun gref hash -> Fmt.pr "%s %s@." hash gref) refs
      | Some (`Ref target) ->
        with_ref (Client.Repo.job_of_ref repo target) job_op
      | Some (`Commit hash) ->
        with_ref (Client.Repo.job_of_commit repo hash) job_op

(* Command-line parsing *)

open Cmdliner

let cap =
  Arg.value @@
  Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None @@
  Arg.info
    ~doc:"The ocaml-ci.cap file."
    ~docv:"CAP"
    ["ci-cap"]

let repo =
  Arg.required @@
  Arg.pos 0 Arg.(some string) None @@
  Arg.info
    ~doc:"The GitHub repository to use (org/name)."
    ~docv:"REPO"
    []

let gref =
  let make_ref s =
    if String.is_prefix ~affix:"refs/pull/" s then (
      match String.cuts ~sep:"/" s with
      | ["refs"; "pull"; pr] -> Ok (`Ref (Fmt.strf "refs/pull/%s/head" pr))
      | _ -> Ok (`Ref s)
    ) else (
      Ok (`Ref s)
    )
  in
  let parse s =
    if not (Stdlib.String.contains s '/') then (
      if String.length s < 6 then
        Error (`Msg "Git reference should start 'refs/' or be a hash at least 6 characters long")
      else Ok (`Commit s)
    ) else if String.is_prefix ~affix:"refs/" s then make_ref s
    else make_ref ("refs/" ^ s)
  in
  let pp f = function
    | `Commit s -> Fmt.string f s
    | `Ref r -> Fmt.string f r
  in
  Arg.conv (parse, pp)

let target =
  Arg.value @@
  Arg.pos 1 Arg.(some gref) None @@
  Arg.info
    ~doc:"The branch, commit or pull request to use. e.g. heads/master or pull/3"
    ~docv:"TARGET"
    []

let job_op =
  let ops = [
    "log", `Show_log;
    "status", `Show_status;
    "cancel", `Cancel;
    "rebuild", `Rebuild;
  ] in
  Arg.value @@
  Arg.pos 2 Arg.(enum ops) `Show_status @@
  Arg.info
    ~doc:"The operation to perform (log, status, cancel or rebuild)."
    ~docv:"METHOD"
    []

(* (cmdliner's [enum] can't cope with functions) *)
let to_fn = function
  | `Cancel -> cancel
  | `Rebuild -> rebuild
  | `Show_log -> show_log
  | `Show_status -> show_status

let cmd =
  let doc = "Client for ocaml-ci" in
  let main ci_uri repo target job_op =
    let job_op = to_fn job_op in
    match Lwt_main.run (main ~ci_uri ~repo ~target ~job_op) with
    | Ok () -> ()
    | Error `Capnp ex -> Fmt.epr "%a@." Capnp_rpc.Error.pp ex; exit 1
    | Error `Msg m -> Fmt.epr "%s@." m; exit 1
  in
  Term.(const main $ cap $ repo $ target $ job_op),
  Term.info "ocaml-ci" ~doc

let () = Term.(exit @@ eval cmd)
