module Db = Current.Db

type t = {
  db : Sqlite3.db;
  record : Sqlite3.stmt;
  lookup : Sqlite3.stmt;
  owner_exists : Sqlite3.stmt;
  repo_exists : Sqlite3.stmt;
  get_job : Sqlite3.stmt;
  list_owners : Sqlite3.stmt;
  list_repos : Sqlite3.stmt;
}

let or_fail label x =
  match x with
  | Sqlite3.Rc.OK -> ()
  | err -> Fmt.failwith "Sqlite3 %s error: %s" label (Sqlite3.Rc.to_string err)

let db = lazy (
  let db = Lazy.force Current.Db.v in
  Sqlite3.exec db "CREATE TABLE IF NOT EXISTS ci_index ( \
                   owner     TEXT NOT NULL, \
                   name      TEXT NOT NULL, \
                   hash      TEXT NOT NULL, \
                   job_id    TEXT NOT NULL, \
                   PRIMARY KEY (owner, name, hash))" |> or_fail "create table";
  let record = Sqlite3.prepare db "INSERT OR REPLACE INTO ci_index \
                                     (owner, name, hash, job_id) \
                                     VALUES (?, ?, ?, ?)" in
  let lookup = Sqlite3.prepare db "SELECT job_id FROM ci_index \
                                     WHERE owner = ? AND name = ? AND hash = ?" in
  let list_owners = Sqlite3.prepare db "SELECT DISTINCT owner FROM ci_index" in
  let list_repos = Sqlite3.prepare db "SELECT DISTINCT name FROM ci_index WHERE owner = ?" in
  let owner_exists = Sqlite3.prepare db "SELECT EXISTS (SELECT 1 FROM ci_index \
                                                        WHERE owner = ?)" in
  let repo_exists = Sqlite3.prepare db "SELECT EXISTS (SELECT 1 FROM ci_index \
                                                       WHERE owner = ? AND name = ?)" in
  let get_job = Sqlite3.prepare db "SELECT job_id FROM ci_index \
                                    WHERE owner = ? AND name = ? AND hash LIKE ?" in
  { db; record; lookup; owner_exists; repo_exists; get_job; list_owners; list_repos }
)

let split_owner_name s =
  match String.split_on_char '/' s with
  | [ owner; name ] -> owner, name
  | _ -> Fmt.failwith "GitHub owner name field should have form 'owner/name', not %S" s

let record ~commit job_id =
  let t = Lazy.force db in
  let owner, name = split_owner_name (Current_github.Api.Commit.owner_name commit) in
  let hash = Current_github.Api.Commit.hash commit in
  match Db.query_some t.lookup Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash ] with
  | Some Sqlite3.Data.[TEXT old] when old = job_id -> ()
  | _ -> Db.exec t.record Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash; TEXT job_id ]

let is_known_owner owner =
  let t = Lazy.force db in
  match Db.query_one t.owner_exists Sqlite3.Data.[ TEXT owner ] with
  | Sqlite3.Data.[ INT x ] -> x = 1L
  | _ -> failwith "owner_exists failed!"

let is_known_repo ~owner ~name =
  let t = Lazy.force db in
  match Db.query_one t.repo_exists Sqlite3.Data.[ TEXT owner; TEXT name ] with
  | Sqlite3.Data.[ INT x ] -> x = 1L
  | _ -> failwith "repo_exists failed!"

let get_job ~owner ~name hash =
  let t = Lazy.force db in
  match Db.query t.get_job Sqlite3.Data.[ TEXT owner; TEXT name; TEXT (hash ^ "%") ] with
  | [] -> Error `Unknown
  | [Sqlite3.Data.[ TEXT job_id ]] -> Ok job_id
  | [_] -> failwith "get_job: invalid result!"
  | _ :: _ :: _ -> Error `Ambiguous

let list_owners () =
  let t = Lazy.force db in
  Db.query t.list_owners []
  |> List.map @@ function
  | Sqlite3.Data.[ TEXT x ] -> x
  | _ -> failwith "list_owners: invalid data returned!"

let list_repos owner =
  let t = Lazy.force db in
  Db.query t.list_repos Sqlite3.Data.[ TEXT owner ]
  |> List.map @@ function
  | Sqlite3.Data.[ TEXT x ] -> x
  | _ -> failwith "list_repos: invalid data returned!"

module Repo_map = Map.Make(Current_github.Repo_id)

let active_refs = ref Repo_map.empty

let set_active_refs ~repo (refs : (string * string) list) =
  active_refs := Repo_map.add repo refs !active_refs

let get_active_refs repo =
  Repo_map.find_opt repo !active_refs |> Option.value ~default:[]
