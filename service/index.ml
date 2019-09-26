module Db = Current.Db

type t = {
  db : Sqlite3.db;
  record : Sqlite3.stmt;
  lookup : Sqlite3.stmt;
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
  { db; record; lookup }
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
