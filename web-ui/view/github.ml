include Git_forge.Make (struct
  let prefix = "github"
  let request_abbrev = "PR"
  let org_url ~org = Printf.sprintf "https://github.com/%s" org
  let repo_url ~org ~repo = Printf.sprintf "https://github.com/%s/%s" org repo

  let branch_url ~org ~repo ref =
    Printf.sprintf "https://github.com/%s/%s/tree/%s" org repo ref

  let commit_url ~org ~repo ~hash =
    Printf.sprintf "https://github.com/%s/%s/commit/%s" org repo hash

  let request_url ~org ~repo id =
    Printf.sprintf "https://github.com/%s/%s/pull/%s" org repo id

  let parse_ref r =
    match Astring.String.cuts ~sep:"/" r with
    | "refs" :: "heads" :: branch ->
        let branch = Astring.String.concat ~sep:"/" branch in
        `Branch branch
    | [ "refs"; "pull"; id; "head" ] ->
        let id = int_of_string id in
        `Request id
    | _ -> `Unknown r

  let ref_path r =
    match Astring.String.cuts ~sep:"/" r with
    | "refs" :: "heads" :: branch ->
        let branch = Astring.String.concat ~sep:"/" branch in
        Ok (Printf.sprintf "branch/%s" branch)
    | [ "refs"; "pull"; id; "head" ] ->
        let id = int_of_string id in
        Ok (Printf.sprintf "pull/%d" id)
    | _ -> Error (Printf.sprintf "Could not parse ref %s" r)
end)
