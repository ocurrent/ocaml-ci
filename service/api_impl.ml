module Rpc = Current_rpc.Impl (Current)
module Raw = Ocaml_ci_api.Raw
module String_map = Map.Make (String)
module Index = Ocaml_ci.Index
module Run_time = Ocaml_ci.Run_time
open Capnp_rpc_lwt

let make_commit ~engine ~owner ~name hash =
  let module Commit = Raw.Service.Commit in
  Commit.local
  @@ object
       inherit Commit.service

       method jobs_impl _params release_param_caps =
         let open Commit.Jobs in
         release_param_caps ();
         let jobs = Index.get_jobs ~owner ~name hash in
         let response, results = Service.Response.create Results.init_pointer in
         let arr = Results.jobs_init results (List.length jobs) in
         let f i (variant, outcome, ts) =
           let open Raw.Builder.JobInfo in
           let slot = Capnp.Array.get arr i in
           variant_set slot variant;
           let queued_at, started_at, finished_at =
             match ts with
             | None -> (None, None, None)
             | Some (Run_time.Queued v) -> (Some v, None, None)
             | Some (Running v) -> (Some v.queued_at, Some v.started_at, None)
             | Some (Finished v) ->
                 (Some v.queued_at, v.started_at, Some v.finished_at)
           in
           let queued_at_t = queued_at_init slot in
           let module S = QueuedAt in
           (match queued_at with
           | None -> S.none_set queued_at_t
           | Some v -> S.ts_set queued_at_t v);
           let started_at_t = started_at_init slot in
           let module S = StartedAt in
           (match started_at with
           | None -> S.none_set started_at_t
           | Some v -> S.ts_set started_at_t v);
           let finished_at_t = finished_at_init slot in
           let module S = FinishedAt in
           (match finished_at with
           | None -> S.none_set finished_at_t
           | Some v -> S.ts_set finished_at_t v);
           let state = state_init slot in
           let module S = State in
           match outcome with
           | `Not_started -> S.not_started_set state
           | `Passed -> S.passed_set state
           | `Aborted -> S.aborted_set state
           | `Active -> S.active_set state
           | `Failed msg -> S.failed_set state msg
         in
         jobs |> List.iteri f;
         Service.return response

       method job_of_variant_impl params release_param_caps =
         let open Commit.JobOfVariant in
         let variant = Params.variant_get params in
         release_param_caps ();
         match Index.get_job ~owner ~name ~hash ~variant with
         | Error `No_such_variant -> Service.fail "No such variant %S" variant
         | Ok None -> Service.fail "No job for variant %S yet" variant
         | Ok (Some id) ->
             let job = Rpc.job ~engine id in
             let response, results =
               Service.Response.create Results.init_pointer
             in
             Results.job_set results (Some job);
             Capability.dec_ref job;
             Service.return response

       method refs_impl _params release_param_caps =
         let open Commit.Refs in
         release_param_caps ();
         let refs =
           Index.get_active_refs { Ocaml_ci.Repo_id.owner; name }
           |> Index.Ref_map.bindings
           |> List.filter_map (fun (name, { Index.hash = h; _ }) ->
                  if h = hash then Some name else None)
         in
         let response, results = Service.Response.create Results.init_pointer in
         Results.refs_set_list results refs |> ignore;
         Service.return response

       method status_impl _params release_param_caps =
         let open Commit.Status in
         release_param_caps ();
         let response, results = Service.Response.create Results.init_pointer in
         (Index.Commit_cache.(find ~owner ~name ~hash |> get_status) |> function
          | `Not_started -> Results.status_set results NotStarted
          | `Pending -> Results.status_set results Pending
          | `Failed -> Results.status_set results Failed
          | `Passed -> Results.status_set results Passed);
         Service.return response

       method message_impl _params release_param_caps =
         let open Commit.Message in
         release_param_caps ();
         let response, results = Service.Response.create Results.init_pointer in
         Results.message_set results
           (Index.get_commit_message
              ~repo:{ Ocaml_ci.Repo_id.owner; name }
              ~hash);
         Service.return response

       method title_impl _params release_param_caps =
         let open Commit.Title in
         release_param_caps ();
         let response, results = Service.Response.create Results.init_pointer in
         let active =
           Index.get_active_refs { Ocaml_ci.Repo_id.owner; name }
           |> Index.Ref_map.bindings
           |> List.filter_map (fun (_, { Index.hash = h; name; _ }) ->
                  if h = hash then Some name else None)
         in
         (match active with
         | [] ->
             Logs.err (fun m -> m "Commit has no associated title: %s" hash);
             raise Not_found
         | title :: _ -> Results.title_set results title);
         Service.return response
     end

let to_build_status =
  let open Raw.Builder.BuildStatus in
  function
  | `Not_started -> NotStarted
  | `Failed -> Failed
  | `Pending -> Pending
  | `Passed -> Passed

let make_repo ~engine ~owner ~name =
  let module Repo = Raw.Service.Repo in
  let commits = ref String_map.empty in
  (* Returned reference is borrowed. Call [inc_ref] if you need to keep it. *)
  let get_commit hash =
    match String_map.find_opt hash !commits with
    | Some x -> x
    | None ->
        let commit = make_commit ~engine ~owner ~name hash in
        commits := String_map.add hash commit !commits;
        commit
  in
  Repo.local
  @@ object
       inherit Repo.service

       method refs_impl _params release_param_caps =
         let open Repo.Refs in
         release_param_caps ();

         let repo_id = { Ocaml_ci.Repo_id.owner; name } in
         let refs = Index.get_active_refs repo_id |> Index.Ref_map.bindings in
         let response, results = Service.Response.create Results.init_pointer in
         let arr = Results.refs_init results (List.length refs) in
         refs
         |> List.iteri
              (fun i (gref, { Index.hash; message; name = ref_name }) ->
                let open Raw.Builder.RefInfo in
                let slot = Capnp.Array.get arr i in
                ref_set slot gref;
                hash_set slot hash;
                let status =
                  to_build_status
                    Index.Commit_cache.(get_status @@ find ~owner ~name ~hash)
                in
                status_set slot status;
                message_set slot message;
                name_set slot ref_name;
                let ref_s =
                  Index.Aggregate.get_ref_state ~repo:repo_id ~ref:gref
                in
                let started_at_t = started_at_init slot in
                (match Index.Aggregate.get_ref_started_at ref_s with
                | None -> StartedAt.none_set started_at_t
                | Some time -> StartedAt.ts_set started_at_t time);
                let ran_for_t = ran_for_init slot in
                match Index.Aggregate.get_ref_ran_for ref_s with
                | None -> RanFor.none_set ran_for_t
                | Some time -> RanFor.ts_set ran_for_t time);
         Service.return response

       method default_ref_impl _params release_param_caps =
         let open Repo.DefaultRef in
         release_param_caps ();
         let default_gref =
           Index.get_default_gref { Ocaml_ci.Repo_id.owner; name }
         in
         let repo_id = { Ocaml_ci.Repo_id.owner; name } in
         let refs = Index.get_active_refs { Ocaml_ci.Repo_id.owner; name } in
         let default_ref = Index.Ref_map.find default_gref refs in
         let response, results = Service.Response.create Results.init_pointer in
         let slot = Results.default_init results in
         (match default_ref with
         | { Index.hash; message; name = ref_name } -> (
             let open Raw.Builder.RefInfo in
             ref_set slot default_gref;
             hash_set slot hash;
             let status =
               to_build_status
                 Index.Commit_cache.(get_status @@ find ~owner ~name ~hash)
             in
             status_set slot status;
             message_set slot message;
             name_set slot ref_name;
             let ref_s =
               Index.Aggregate.get_ref_state ~repo:repo_id ~ref:default_gref
             in
             let started_at_t = started_at_init slot in
             (match Index.Aggregate.get_ref_started_at ref_s with
             | None -> StartedAt.none_set started_at_t
             | Some time -> StartedAt.ts_set started_at_t time);
             let ran_for_t = ran_for_init slot in
             match Index.Aggregate.get_ref_ran_for ref_s with
             | None -> RanFor.none_set ran_for_t
             | Some time -> RanFor.ts_set ran_for_t time));
         Service.return response

       method obsolete_refs_of_commit_impl _ release_param_caps =
         release_param_caps ();
         Service.fail "This method no longer exists"

       method commit_of_ref_impl params release_param_caps =
         let open Repo.CommitOfRef in
         let gref = Params.ref_get params in
         release_param_caps ();
         let refs = Index.get_active_refs { Ocaml_ci.Repo_id.owner; name } in
         match Index.Ref_map.find_opt gref refs with
         | None ->
             Service.fail "@[<v2>Unknown ref %S. Options are:@,%a@]" gref
               Fmt.(Dump.list string)
               (List.map fst (Index.Ref_map.bindings refs))
         | Some { Index.hash; _ } ->
             let commit = get_commit hash in
             let response, results =
               Service.Response.create Results.init_pointer
             in
             Results.commit_set results (Some commit);
             Service.return response

       method commit_of_hash_impl params release_param_caps =
         let open Repo.CommitOfHash in
         let hash = Params.hash_get params in
         release_param_caps ();
         match Index.get_full_hash ~owner ~name hash with
         | Error `Ambiguous -> Service.fail "Ambiguous commit hash %S" hash
         | Error `Invalid -> Service.fail "Invalid Git hash %S" hash
         | Error `Unknown -> Service.fail "Unknown Git hash %S" hash
         | Ok hash ->
             let commit = get_commit hash in
             let response, results =
               Service.Response.create Results.init_pointer
             in
             Results.commit_set results (Some commit);
             Service.return response

       method history_of_ref_impl params release_param_caps =
         let open Repo.HistoryOfRef in
         let gref = Params.ref_get params in
         release_param_caps ();
         let history = Index.get_build_history ~owner ~name ~gref in
         let response, results = Service.Response.create Results.init_pointer in
         let arr = Results.refs_init results (List.length history) in
         history
         |> List.iteri
              (fun
                i
                ( hash,
                  _build_number,
                  status,
                  started_at,
                  _total_ran_for,
                  ran_for,
                  _total_queued_for,
                  message )
              ->
                let open Raw.Builder.RefInfo in
                let slot = Capnp.Array.get arr i in
                ref_set slot gref;
                hash_set slot hash;
                message_set slot message;
                let status =
                  to_build_status
                  @@ Result.get_ok
                  @@ Index.int_to_status
                  @@ Int64.to_int status
                in
                status_set slot status;
                let started_at_t = started_at_init slot in
                (match started_at with
                | None -> StartedAt.none_set started_at_t
                | Some time -> StartedAt.ts_set started_at_t time);
                let ran_for_t = ran_for_init slot in
                match ran_for with
                | None -> RanFor.none_set ran_for_t
                | Some time -> RanFor.ts_set ran_for_t time);
         Service.return response

       method obsolete_job_of_commit_impl _ release_param_caps =
         release_param_caps ();
         Service.fail "This method no longer exists"

       method obsolete_job_of_ref_impl _ release_param_caps =
         release_param_caps ();
         Service.fail "This method no longer exists"
     end

let make_org ~engine owner =
  let module Org = Raw.Service.Org in
  let repos = ref String_map.empty in
  (* Returned reference is borrowed. Call [inc_ref] if you need to keep it. *)
  let get_repo name =
    match String_map.find_opt name !repos with
    | Some repo -> Some repo
    | None ->
        let active_repos = Index.get_active_repos ~owner in
        if Index.Repo_set.mem name active_repos then (
          let repo = make_repo ~engine ~owner ~name in
          repos := String_map.add name repo !repos;
          Some repo)
        else None
  in
  Org.local
  @@ object
       inherit Org.service

       method repo_impl params release_param_caps =
         let open Org.Repo in
         let name = Params.name_get params in
         release_param_caps ();
         match get_repo name with
         | None -> Service.fail "Invalid GitHub repo %S/%S" owner name
         | Some repo ->
             let response, results =
               Service.Response.create Results.init_pointer
             in
             Results.repo_set results (Some repo);
             Service.return response

       method repos_impl _params release_param_caps =
         let open Org.Repos in
         release_param_caps ();
         let response, results = Service.Response.create Results.init_pointer in
         let repos = Index.get_active_repos ~owner |> Index.Repo_set.elements in
         let arr = Results.repos_init results (List.length repos) in
         repos
         |> List.iteri (fun i name ->
                let open Raw.Builder.RepoInfo in
                let slot = Capnp.Array.get arr i in
                name_set slot name;
                let repo_id = { Ocaml_ci.Repo_id.owner; name } in
                let refs = Index.get_active_refs repo_id in
                let repo = Index.Aggregate.get_repo_state ~repo:repo_id in
                let default_ref =
                  Index.get_default_gref { Ocaml_ci.Repo_id.owner; name }
                in
                let hash, status =
                  match Index.Ref_map.find default_ref refs with
                  | { Index.hash; _ } ->
                      ( hash,
                        to_build_status
                          Index.Commit_cache.(
                            get_status @@ find ~owner ~name ~hash) )
                in
                main_hash_set slot hash;
                main_state_set slot status;
                let last_updated_t = main_last_updated_init slot in
                match Index.Aggregate.get_repo_started_at repo with
                | None -> MainLastUpdated.none_set last_updated_t
                | Some time -> MainLastUpdated.ts_set last_updated_t time);
         Service.return response
     end

let make_ci ~engine =
  let module CI = Raw.Service.CI in
  let orgs = ref String_map.empty in
  (* Returned reference is borrowed. Call [inc_ref] if you need to keep it. *)
  let get_org owner =
    match String_map.find_opt owner !orgs with
    | Some org -> Some org
    | None ->
        if Index.Owner_set.mem owner (Index.get_active_owners ()) then (
          let org = make_org ~engine owner in
          orgs := String_map.add owner org !orgs;
          Some org)
        else None
  in
  CI.local
  @@ object
       inherit CI.service

       method org_impl params release_param_caps =
         let open CI.Org in
         let owner = Params.owner_get params in
         release_param_caps ();
         match get_org owner with
         | None -> Service.fail "Invalid GitHub owner %S" owner
         | Some org ->
             let response, results =
               Service.Response.create Results.init_pointer
             in
             Results.org_set results (Some org);
             Service.return response

       method orgs_impl _params release_param_caps =
         let open CI.Orgs in
         release_param_caps ();
         let response, results = Service.Response.create Results.init_pointer in
         let owners = Index.get_active_owners () |> Index.Owner_set.elements in
         Results.orgs_set_list results owners |> ignore;
         Service.return response
     end
