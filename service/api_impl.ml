module Rpc = Current_rpc.Impl(Current)
module Raw = Ocaml_ci_api.Raw

module String_map = Map.Make(String)

open Capnp_rpc_lwt

let is_valid_hash hash =
  let open Astring in
  String.length hash >= 6 && String.for_all Char.Ascii.is_alphanum hash

let make_repo ~engine ~owner ~name =
  let module Repo = Raw.Service.Repo in
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

    method refs_of_commit_impl params release_param_caps =
      let open Repo.RefsOfCommit in
      let hash = Params.hash_get params in
      release_param_caps ();
      let refs =
        Index.get_active_refs { Current_github.Repo_id.owner; name }
        |> List.filter_map (fun (name, h) -> if h = hash then Some name else None)
      in
      let response, results = Service.Response.create Results.init_pointer in
      Results.refs_set_list results refs |> ignore;
      Service.return response

    method job_of_commit_impl params release_param_caps =
      let open Repo.JobOfCommit in
      let hash = Params.hash_get params in
      release_param_caps ();
      if not (is_valid_hash hash) then
        Service.fail "Invalid Git hash %S" hash
      else (
        match Index.get_job ~owner ~name hash with
        | Ok id ->
          let job = Rpc.job ~engine id in
          let response, results = Service.Response.create Results.init_pointer in
          Results.job_set results (Some job);
          Capability.dec_ref job;
          Service.return response
        | Error `Unknown ->
          Service.fail "Invalid job %S/%S commit hash %S" owner name hash
        | Error `Ambiguous ->
          Service.fail "Ambiguous commit hash %S" hash
      )

    method job_of_ref_impl params release_param_caps =
      let open Repo.JobOfRef in
      let gref = Params.ref_get params in
      release_param_caps ();
      let refs = Index.get_active_refs { Current_github.Repo_id.owner; name } in
      match List.assoc_opt gref refs with
      | None -> Service.fail "@[<v2>Unknown ref %S. Options are:@,%a@]" gref
                  Fmt.(Dump.list string) (List.map fst refs)
      | Some hash ->
        match Index.get_job ~owner ~name hash with
        | Ok id ->
          let job = Rpc.job ~engine id in
          let response, results = Service.Response.create Results.init_pointer in
          Results.job_set results (Some job);
          Capability.dec_ref job;
          Service.return response
        | Error `Unknown ->
          Service.fail "Invalid job %S/%S commit hash %S" owner name hash
        | Error `Ambiguous ->
          Service.fail "Ambiguous commit hash %S" hash
  end

let make_org ~engine owner =
  let module Org = Raw.Service.Org in
  let repos = ref String_map.empty in
  let get_repo name =
    match String_map.find_opt name !repos with
    | Some repo -> Capability.inc_ref repo; Some repo
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
  let get_org owner =
    match String_map.find_opt owner !orgs with
    | Some org -> Capability.inc_ref org; Some org
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
