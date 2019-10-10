module Rpc = Current_rpc.Impl(Current)
module Raw = Ocaml_ci_api.Raw

module String_map = Map.Make(String)

open Capnp_rpc_lwt

let make_commit ~engine ~owner ~name hash =
  let module Commit = Raw.Service.Commit in
  Commit.local @@ object
    inherit Commit.service

    method jobs_impl _params release_param_caps =
      let open Commit.Jobs in
      release_param_caps ();
      let jobs = Index.get_jobs ~owner ~name hash in
      let response, results = Service.Response.create Results.init_pointer in
      let arr = Results.jobs_init results (List.length jobs) in
      jobs |> List.iteri (fun i (variant, _job_id) ->
          let slot = Capnp.Array.get arr i in
          Raw.Builder.JobInfo.variant_set slot variant;
        );
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
        let response, results = Service.Response.create Results.init_pointer in
        Results.job_set results (Some job);
        Capability.dec_ref job;
        Service.return response

    method refs_impl _params release_param_caps =
      let open Commit.Refs in
      release_param_caps ();
      let refs =
        Index.get_active_refs { Current_github.Repo_id.owner; name }
        |> List.filter_map (fun (name, h) -> if h = hash then Some name else None)
      in
      let response, results = Service.Response.create Results.init_pointer in
      Results.refs_set_list results refs |> ignore;
      Service.return response
  end

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
  Repo.local @@ object
    inherit Repo.service

    method refs_impl _params release_param_caps =
      let open Repo.Refs in
      release_param_caps ();
      let refs = Index.get_active_refs { Current_github.Repo_id.owner; name } in
      let response, results = Service.Response.create Results.init_pointer in
      let arr = Results.refs_init results (List.length refs) in
      refs |> List.iteri (fun i (gref, hash) ->
          let slot = Capnp.Array.get arr i in
          Raw.Builder.RefInfo.ref_set slot gref;
          Raw.Builder.RefInfo.hash_set slot hash;
        );
      Service.return response

    method deprecated_refs_of_commit_impl params release_param_caps =
      let open Repo.DeprecatedRefsOfCommit in
      let hash = Params.hash_get params in
      release_param_caps ();
      let refs =
        Index.get_active_refs { Current_github.Repo_id.owner; name }
        |> List.filter_map (fun (name, h) -> if h = hash then Some name else None)
      in
      let response, results = Service.Response.create Results.init_pointer in
      Results.refs_set_list results refs |> ignore;
      Service.return response

    method commit_of_ref_impl params release_param_caps =
      let open Repo.CommitOfRef in
      let gref = Params.ref_get params in
      release_param_caps ();
      let refs = Index.get_active_refs { Current_github.Repo_id.owner; name } in
      match List.assoc_opt gref refs with
      | None -> Service.fail "@[<v2>Unknown ref %S. Options are:@,%a@]" gref
                  Fmt.(Dump.list string) (List.map fst refs)
      | Some hash ->
        let commit = get_commit hash in
        let response, results = Service.Response.create Results.init_pointer in
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
        let response, results = Service.Response.create Results.init_pointer in
        Results.commit_set results (Some commit);
        Service.return response

    method deprecated_job_of_commit_impl params release_param_caps =
      let open Repo.DeprecatedJobOfCommit in
      let hash = Params.hash_get params in
      release_param_caps ();
      match Index.get_full_hash ~owner ~name hash with
      | Error `Ambiguous -> Service.fail "Ambiguous commit hash %S" hash
      | Error `Invalid -> Service.fail "Invalid Git hash %S" hash
      | Error `Unknown -> Service.fail "Unknown Git hash %S" hash
      | Ok hash ->
        match Index.get_job ~owner ~name ~hash ~variant:"alpine-3.10-ocaml-4.08" with
        | Error `No_such_variant -> Service.fail "No such job"
        | Ok None -> Service.fail "Job not started yet"
        | Ok (Some id) ->
          let job = Rpc.job ~engine id in
          let response, results = Service.Response.create Results.init_pointer in
          Results.job_set results (Some job);
          Capability.dec_ref job;
          Service.return response

    method deprecated_job_of_ref_impl params release_param_caps =
      let open Repo.DeprecatedJobOfRef in
      let gref = Params.ref_get params in
      release_param_caps ();
      let refs = Index.get_active_refs { Current_github.Repo_id.owner; name } in
      match List.assoc_opt gref refs with
      | None -> Service.fail "@[<v2>Unknown ref %S. Options are:@,%a@]" gref
                  Fmt.(Dump.list string) (List.map fst refs)
      | Some hash ->
        match Index.get_job ~owner ~name ~hash ~variant:"alpine-3.10-ocaml-4.08" with
        | Error `No_such_variant -> Service.fail "No such job"
        | Ok None -> Service.fail "Job not started yet"
        | Ok (Some id) ->
          let job = Rpc.job ~engine id in
          let response, results = Service.Response.create Results.init_pointer in
          Results.job_set results (Some job);
          Capability.dec_ref job;
          Service.return response
  end

let make_org ~engine owner =
  let module Org = Raw.Service.Org in
  let repos = ref String_map.empty in
  (* Returned reference is borrowed. Call [inc_ref] if you need to keep it. *)
  let get_repo name =
    match String_map.find_opt name !repos with
    | Some repo -> Some repo
    | None ->
      if Index.is_known_repo ~owner ~name then (
        let repo = make_repo ~engine ~owner ~name in
        repos := String_map.add name repo !repos;
        Some repo
      ) else None
  in
  Org.local @@ object
    inherit Org.service

    method repo_impl params release_param_caps =
      let open Org.Repo in
      let name = Params.name_get params in
      release_param_caps ();
      match get_repo name with
      | None -> Service.fail "Invalid GitHub repo %S/%S" owner name
      | Some repo ->
        let response, results = Service.Response.create Results.init_pointer in
        Results.repo_set results (Some repo);
        Service.return response

    method repos_impl _params release_param_caps =
      let open Org.Repos in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.repos_set_list results (Index.list_repos owner) |> ignore;
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
      if Index.is_known_owner owner then (
        let org = make_org ~engine owner in
        orgs := String_map.add owner org !orgs;
        Some org
      ) else None
  in
  CI.local @@ object
    inherit CI.service

    method org_impl params release_param_caps =
      let open CI.Org in
      let owner = Params.owner_get params in
      release_param_caps ();
      match get_org owner with
      | None -> Service.fail "Invalid GitHub owner %S" owner
      | Some org ->
        let response, results = Service.Response.create Results.init_pointer in
        Results.org_set results (Some org);
        Service.return response

    method orgs_impl _params release_param_caps =
      let open CI.Orgs in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.orgs_set_list results (Index.list_owners ()) |> ignore;
      Service.return response
  end
