let src = Logs.Src.create "ocaml_ci.index" ~doc:"ocaml-ci indexer"

open Current.Syntax
module Log = (val Logs.src_log src : Logs.LOG)
module Db = Current.Db
module Job_map = Astring.String.Map

module Migration = struct
  open Lwt.Infix

  let ( >>!= ) = Lwt_result.Infix.( >>= )

  type t = string

  let id = "ocaml-ci-db"

  module Key = struct
    type t = float

    let digest = Float.to_string
    let pp f t = Fmt.pf f "Date %f" t
  end

  module Value = Current.Unit

  let to_current_error = function
    | Ok () -> Lwt_result.return ()
    | Error err ->
        let msg =
          match err with
          | `Unknown_driver s ->
              Printf.sprintf "omigrate: unknown driver (%s)" s
          | `Bad_uri s -> Printf.sprintf "omigrate: bad uri (%s)" s
          | `Invalid_source s ->
              Printf.sprintf "omigrate: invalid source (%s)" s
        in
        Lwt_result.fail (`Msg msg)

  let to_lwt_exn = function
    | Ok () -> Lwt_result.return ()
    | Error err ->
        let msg =
          match err with
          | `Unknown_driver s ->
              Printf.sprintf "omigrate: unknown driver (%s)" s
          | `Bad_uri s -> Printf.sprintf "omigrate: bad uri (%s)" s
          | `Invalid_source s ->
              Printf.sprintf "omigrate: invalid source (%s)" s
        in
        Lwt_result.fail (failwith msg)

  let migrate source =
    let db_dir = Current.state_dir "db" in
    let db_path = Fpath.(to_string (db_dir / "sqlite.db")) in
    let database = Uri.(make ~scheme:"sqlite3" ~path:db_path () |> to_string) in
    Omigrate.create ~database >>!= fun () -> Omigrate.up ~source ~database ()

  let build source job _date =
    Current.Job.start job ~level:Current.Level.Dangerous >>= fun () ->
    Current.Job.log job "Running migration from migrations/";
    migrate source >>= to_current_error

  let pp = Key.pp
  let auto_cancel = true

  (* Functions for a test purpose *)

  let init () =
    let source =
      let pwd = Fpath.v (Sys.getcwd ()) in
      Fpath.(to_string (pwd / "migrations"))
    in
    migrate source >>= to_lwt_exn |> Lwt_result.get_exn
end

module Migration_cache = Current_cache.Make (Migration)

let migrate path =
  Current.component "migrations"
  |> let> date = Current.return (Unix.time ()) in
     Migration_cache.get path date

type t = {
  record_job : Sqlite3.stmt;
  remove : Sqlite3.stmt;
  get_jobs : Sqlite3.stmt;
  get_job : Sqlite3.stmt;
  get_job_ids : Sqlite3.stmt;
  full_hash : Sqlite3.stmt;
  record_job_summary : Sqlite3.stmt;
  get_build_summary_by_commit : Sqlite3.stmt;
  get_build_summary_by_gref : Sqlite3.stmt;
}

type job_state =
  [ `Not_started | `Active | `Failed of string | `Passed | `Aborted ]
[@@deriving show]

type build_status = [ `Not_started | `Pending | `Failed | `Passed ]

let is_valid_hash hash =
  let open Astring in
  String.length hash >= 6 && String.for_all Char.Ascii.is_alphanum hash

let db =
  lazy
    (let db = Lazy.force Current.Db.v in
     Current_cache.Db.init ();
     let record_job =
       Sqlite3.prepare db
         "INSERT OR REPLACE INTO ci_build_index (owner, name, hash, variant, \
          gref, job_id) VALUES (?, ?, ?, ?, ?, ?)"
     and remove =
       Sqlite3.prepare db
         "DELETE FROM ci_build_index WHERE owner = ? AND name = ? AND hash = ? \
          AND variant = ?"
     and get_jobs =
       Sqlite3.prepare db
         "SELECT ci_build_index.variant, ci_build_index.job_id, cache.ok, \
          cache.outcome FROM ci_build_index LEFT JOIN cache ON \
          ci_build_index.job_id = cache.job_id WHERE ci_build_index.owner = ? \
          AND ci_build_index.name = ? AND ci_build_index.hash = ?"
     and get_job =
       Sqlite3.prepare db
         "SELECT job_id FROM ci_build_index WHERE owner = ? AND name = ? AND \
          hash = ? AND variant = ?"
     and get_job_ids =
       Sqlite3.prepare db
         "SELECT variant, job_id FROM ci_build_index WHERE owner = ? AND name \
          = ? AND hash = ?"
     and full_hash =
       Sqlite3.prepare db
         "SELECT DISTINCT hash FROM ci_build_index WHERE owner = ? AND name = \
          ? AND hash LIKE ?"
     and record_job_summary =
       Sqlite3.prepare db
         "INSERT INTO ci_build_summary (owner, name, hash, gref, build_number, \
          status, started_at, total_ran_for, ran_for, created_at, message) \
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     and get_build_summary_by_commit =
       Sqlite3.prepare db
         "SELECT build_number, status, started_at, total_ran_for, ran_for, \
          total_queued_for, message FROM ci_build_summary WHERE owner = ? AND \
          name = ? AND hash = ? ORDER BY build_number DESC"
     and get_build_summary_by_gref =
       Sqlite3.prepare db
         "SELECT hash, build_number, status, started_at, total_ran_for, \
          ran_for, total_queued_for, message FROM ci_build_summary c1 WHERE \
          owner = ? AND name = ? AND gref = ? AND build_number = (SELECT \
          MAX(build_number) FROM ci_build_summary c2 WHERE c1.owner = c2.owner \
          AND c1.name = c2.name AND c1.hash = c2.hash) ORDER BY created_at \
          DESC"
     in
     {
       record_job;
       remove;
       get_jobs;
       get_job;
       get_job_ids;
       full_hash;
       record_job_summary;
       get_build_summary_by_commit;
       get_build_summary_by_gref;
     })

let init () = Lwt.map (fun () -> ignore (Lazy.force db)) (Migration.init ())

let get_job_ids_with_variant t ~owner ~name ~hash =
  Db.query t.get_job_ids Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash ]
  |> List.map @@ function
     | Sqlite3.Data.[ TEXT variant; NULL ] -> (variant, None)
     | Sqlite3.Data.[ TEXT variant; TEXT id ] -> (variant, Some id)
     | row -> Fmt.failwith "get_job_ids: invalid row %a" Db.dump_row row

let some_of_nullable_float =
  let open Sqlite3.Data in
  function FLOAT v -> Some v | NULL -> None | _ -> raise Not_found

let get_build_history ~owner ~name ~gref =
  let f = function
    | Sqlite3.Data.
        [
          TEXT hash;
          INT build_number;
          INT status;
          started_at;
          total_ran_for;
          ran_for;
          FLOAT total_queued_for;
          TEXT message;
        ] as row -> (
        try
          ( hash,
            build_number,
            status,
            some_of_nullable_float started_at,
            some_of_nullable_float total_ran_for,
            some_of_nullable_float ran_for,
            total_queued_for,
            message )
        with Not_found ->
          Fmt.failwith "get_job_summary: invalid row %a" Db.dump_row row)
    | row -> Fmt.failwith "get_job_summary: invalid row %a" Db.dump_row row
  in
  let t = Lazy.force db in
  Db.query t.get_build_summary_by_gref
    Sqlite3.Data.[ TEXT owner; TEXT name; TEXT gref ]
  |> List.map @@ f

let get_latest_build_number t ~owner ~name ~hash =
  let build_numbers =
    Db.query t.get_build_summary_by_commit
      Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash ]
    |> List.map @@ function
       | Sqlite3.Data.[ INT build_number; _; _; _; _; _; _ ] -> build_number
       | row ->
           Fmt.failwith "get_latest_build_number: invalid row %a" Db.dump_row
             row
  in
  match build_numbers with [] -> 0 | x :: _ -> Int64.to_int x

let get_status_by_commit ~owner ~name ~hash =
  let statuses =
    let t = Lazy.force db in
    Db.query t.get_build_summary_by_commit
      Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash ]
    |> List.map @@ function
       | Sqlite3.Data.[ _; INT status; started_at; _; ran_for; _; _ ] ->
           ( status,
             some_of_nullable_float started_at,
             some_of_nullable_float ran_for )
       | row ->
           Fmt.failwith "get_status_by_commit: invalid row %a" Db.dump_row row
  in
  match statuses with
  | [] -> (-1, None, None)
  | (x, s, r) :: _ -> (Int64.to_int x, s, r)

module Owner_set = Set.Make (String)

let active_owners = ref Owner_set.empty
let set_active_owners x = active_owners := x
let get_active_owners () = !active_owners

module Owner_map = Map.Make (String)
module Repo_set = Set.Make (String)

let active_repos = ref Owner_map.empty

let set_active_repos ~owner x =
  active_repos := Owner_map.add owner x !active_repos

let get_active_repos ~owner =
  Owner_map.find_opt owner !active_repos |> Option.value ~default:Repo_set.empty

let get_active_owners_with_repo_count () =
  Owner_map.bindings !active_repos
  |> List.map (fun (name, repos) -> (name, Repo_set.cardinal repos))

module Repo_map = Map.Make (Repo_id)
module Ref_map = Map.Make (String)

type status = [ `Failed | `Pending | `Not_started | `Passed ] [@@deriving show]

let status_to_int = function
  | `Failed -> 0
  | `Pending -> 1
  | `Not_started -> 2
  | `Passed -> 3

let int_to_status = function
  | 0 -> Ok `Failed
  | 1 -> Ok `Pending
  | 2 -> Ok `Not_started
  | 3 -> Ok `Passed
  | _ -> Error "Unrecognised status: %d"

module Aggregate = struct
  type ref_state = {
    s : status;
    started_at : float option;
    ran_for : float option;
  }

  type repo_state = {
    default_ref : string option;
    ref_states : ref_state Ref_map.t;
  }

  let get_default_ref (s : repo_state) =
    s.default_ref
    |> Option.map (fun d -> Ref_map.find_opt d s.ref_states)
    |> Option.join

  let get_ref_status (s : ref_state) = s.s
  let get_ref_started_at (s : ref_state) = s.started_at
  let get_ref_ran_for (s : ref_state) = s.ran_for

  let get_repo_status (s : repo_state) =
    match get_default_ref s with None -> `Not_started | Some s -> s.s

  let get_repo_started_at (s : repo_state) =
    get_default_ref s |> Option.map (fun s -> s.started_at) |> Option.join

  let state : repo_state Repo_map.t ref = ref Repo_map.empty

  let set_ref_state ~repo ~gref s started_at ran_for =
    let s_ref = { s; started_at; ran_for } in
    let s_repo =
      match Repo_map.find_opt repo !state with
      | None ->
          { default_ref = None; ref_states = Ref_map.singleton gref s_ref }
      | Some { default_ref; ref_states } ->
          { default_ref; ref_states = Ref_map.add gref s_ref ref_states }
    in
    state := Repo_map.add repo s_repo !state

  let get_ref_state ~repo ~ref =
    let default_ref = { s = `Not_started; started_at = None; ran_for = None } in
    match Repo_map.find_opt repo !state with
    | None -> default_ref
    | Some { ref_states; _ } ->
        Ref_map.find_opt ref ref_states |> Option.value ~default:default_ref

  let get_repo_state ~repo =
    let default_repo = { default_ref = None; ref_states = Ref_map.empty } in
    Repo_map.find_opt repo !state |> Option.value ~default:default_repo

  let get_repo_default_ref repo = repo.default_ref

  let set_repo_default_ref ~repo default_ref =
    let s_repo =
      match Repo_map.find_opt repo !state with
      | None -> { default_ref = Some default_ref; ref_states = Ref_map.empty }
      | Some { ref_states; _ } -> { default_ref = Some default_ref; ref_states }
    in
    state := Repo_map.add repo s_repo !state
end

type ref_info = { hash : string; message : string; name : string }
[@@deriving show]

type repo_info = { default_gref : string; refs : ref_info Ref_map.t }

let active_refs : repo_info Repo_map.t ref = ref Repo_map.empty

let set_active_refs ~repo refs default_gref =
  active_refs := Repo_map.add repo { refs; default_gref } !active_refs;
  Aggregate.set_repo_default_ref ~repo default_gref

let get_active_refs repo =
  Repo_map.find_opt repo !active_refs |> function
  | Some { refs; _ } -> refs
  | None -> Ref_map.empty

let get_commit_message ~repo ~hash =
  let results =
    get_active_refs repo
    |> Ref_map.bindings
    |> List.filter_map (fun (_, { hash = h; message; _ }) ->
           if h = hash then Some message else None)
  in
  match results with
  | [] ->
      Logs.info (fun m -> m "Commit has no associated message: %s" hash);
      hash
  | message :: _ -> message

let get_default_gref repo =
  Repo_map.find_opt repo !active_refs |> function
  | Some { default_gref; _ } -> default_gref
  | None -> raise Not_found

module Commit_cache = struct
  type key = { owner : string; repo : string; hash : string }

  module Commit_map = Map.Make (struct
    type t = key

    let compare { owner = o0; repo = r0; hash = h0 }
        { owner = o1; repo = r1; hash = h1 } =
      let res = String.compare o0 o1 in
      if res != 0 then res
      else
        let res = String.compare r0 r1 in
        if res != 0 then res else String.compare h0 h1
  end)

  type commit_state = {
    s : status;
    started_at : float option;
    ran_for : float option;
  }

  let state : commit_state Commit_map.t ref = ref Commit_map.empty
  let get_status s = s.s
  let get_started_at s = s.started_at
  let get_ran_for s = s.ran_for

  let add ~owner ~name ~hash ~gref status started_at ran_for =
    let v = { s = status; started_at; ran_for } in
    state := Commit_map.add { owner; repo = name; hash } v !state;
    Aggregate.set_ref_state ~repo:{ owner; name } ~gref status started_at
      ran_for

  let find ~owner ~name ~hash =
    let cache_result =
      Commit_map.find_opt { owner; repo = name; hash } !state
    in
    match cache_result with
    | Some result -> result
    | None -> (
        (* Cache miss - fall back to the db *)
        let status, started_at, ran_for =
          get_status_by_commit ~owner ~name ~hash
        in
        match int_to_status status with
        | Ok status ->
            (* FIXME: We don't know the gref here so we can't use the add method. So, we're not
               updating the Aggregate ref_state. What to do? *)
            state :=
              Commit_map.add
                { owner; repo = name; hash }
                { s = status; started_at; ran_for }
                !state;
            { s = status; started_at; ran_for }
        | Error a ->
            Log.err (fun f ->
                f
                  "Failed to get status of commit %s for owner %s repo %s. \
                   Error: %s"
                  hash owner name a);
            raise Not_found)

  let commit_state_from_build_summary ~hash ~build_number ~status ~started_at
      ~total_ran_for ~ran_for ~total_queued_for =
    ignore hash;
    ignore build_number;
    ignore total_ran_for;
    ignore total_queued_for;
    let s = Result.get_ok @@ int_to_status @@ Int64.to_int status in
    { s; started_at; ran_for }
end

let _record_build_summary t ~owner ~name ~hash ~gref ~status ~commit_message
    ~(variants_timestamps : (string * Run_time.timestamps option) list) =
  let ts = List.filter_map snd variants_timestamps in
  let first_queued_at = Run_time.first_step_queued_at ts in
  let build_ran_for = Run_time.build_ran_for variants_timestamps in
  Commit_cache.add ~owner ~name ~hash ~gref status
    (Result.to_option first_queued_at)
    (Some build_ran_for);
  let v status_int =
    let build_created_at =
      Run_time.first_step_queued_at ts
      |> Result.to_option
      |> Option.value ~default:0.
    in
    let total_run_time = Run_time.total_of_run_times ~build_created_at ts in
    let current_time = Unix.gettimeofday () in
    let build_number = 1 + get_latest_build_number t ~owner ~name ~hash in
    Db.exec t.record_job_summary
      Sqlite3.Data.
        [
          TEXT owner;
          TEXT name;
          TEXT hash;
          TEXT gref;
          INT (Int64.of_int build_number);
          INT (Int64.of_int status_int);
          FLOAT build_created_at;
          FLOAT total_run_time;
          FLOAT build_ran_for;
          FLOAT current_time;
          TEXT commit_message;
        ]
  in
  v (status_to_int status)

let record ~repo ~hash ~status ~gref jobs =
  let { Repo_id.owner; name } = repo in
  let t = Lazy.force db in
  let jobs_m = Job_map.of_list jobs in
  let previous =
    get_job_ids_with_variant t ~owner ~name ~hash |> Job_map.of_list
  in
  let merge variant prev job =
    let set job_id =
      Log.info (fun f ->
          f "@[<h>Index.record %s/%s %s %s -> %a@]" owner name
            (Astring.String.with_range ~len:6 hash)
            variant
            Fmt.(option ~none:(any "-") string)
            job_id);
      match job_id with
      | None ->
          Db.exec t.record_job
            Sqlite3.Data.
              [
                TEXT owner; TEXT name; TEXT hash; TEXT variant; TEXT gref; NULL;
              ]
      | Some id ->
          Db.exec t.record_job
            Sqlite3.Data.
              [
                TEXT owner;
                TEXT name;
                TEXT hash;
                TEXT variant;
                TEXT gref;
                TEXT id;
              ]
    in
    let update j1 j2 =
      match (j1, j2) with
      | Some j1, Some j2 when j1 = j2 -> ()
      | None, None -> ()
      | _, j2 -> set j2
    in
    let remove () =
      Log.info (fun f ->
          f "@[<h>Index.record %s/%s %s %s REMOVED@]" owner name
            (Astring.String.with_range ~len:6 hash)
            variant);
      Db.exec t.remove
        Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash; TEXT variant ]
    in
    (match (prev, job) with
    | Some j1, Some j2 -> update j1 j2
    | None, Some j2 -> set j2
    | Some _, None -> remove ()
    | None, None -> assert false);
    None
  in
  let (_ : [ `Empty ] Job_map.t) = Job_map.merge merge previous jobs_m in
  let variants_timestamps =
    List.map
      (fun (variant, job_id) ->
        let id =
          Option.map (fun id -> Run_time.timestamps_of_job id) job_id
          |> Option.join
        in
        (variant, id))
      jobs
  in
  let commit_message = get_commit_message ~repo ~hash in
  _record_build_summary t ~owner ~name ~hash ~gref ~variants_timestamps ~status
    ~commit_message

let get_full_hash ~owner ~name short_hash =
  let t = Lazy.force db in
  if is_valid_hash short_hash then
    match
      Db.query t.full_hash
        Sqlite3.Data.[ TEXT owner; TEXT name; TEXT (short_hash ^ "%") ]
    with
    | [] -> Error `Unknown
    | [ Sqlite3.Data.[ TEXT hash ] ] -> Ok hash
    | [ _ ] -> failwith "full_hash: invalid result!"
    | _ :: _ :: _ -> Error `Ambiguous
  else Error `Invalid

let get_jobs ~owner ~name hash =
  let t = Lazy.force db in
  Db.query t.get_jobs Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash ]
  |> List.map @@ function
     | Sqlite3.Data.[ TEXT variant; TEXT job_id; NULL; NULL ] ->
         let outcome =
           if Current.Job.lookup_running job_id = None then `Aborted
           else `Active
         in
         let ts = Run_time.timestamps_of_job job_id in
         (variant, outcome, ts)
     | Sqlite3.Data.[ TEXT variant; TEXT job_id; INT ok; BLOB outcome ] ->
         let outcome = if ok = 1L then `Passed else `Failed outcome in
         let ts = Run_time.timestamps_of_job job_id in
         (variant, outcome, ts)
     | Sqlite3.Data.[ TEXT variant; NULL; NULL; NULL ] ->
         (variant, `Not_started, None)
     | row -> Fmt.failwith "get_jobs: invalid result: %a" Db.dump_row row

let get_job ~owner ~name ~hash ~variant =
  let t = Lazy.force db in
  match
    Db.query_some t.get_job
      Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash; TEXT variant ]
  with
  | None -> Error `No_such_variant
  | Some Sqlite3.Data.[ TEXT id ] -> Ok (Some id)
  | Some Sqlite3.Data.[ NULL ] -> Ok None
  | _ -> failwith "get_job: invalid result!"

let get_job_ids ~owner ~name ~hash =
  let t = Lazy.force db in
  get_job_ids_with_variant t ~owner ~name ~hash |> List.filter_map snd
