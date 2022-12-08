include Git_forge.Make (struct
  let prefix = "gitlab"
  let request_abbrev = "MR"
  let request_prefix = "merge-request"
  let org_url ~org = Printf.sprintf "https://gitlab.com/%s" org
  let repo_url ~org ~repo = Printf.sprintf "https://gitlab.com/%s/%s" org repo

  let commit_url ~org ~repo ~hash =
    Printf.sprintf "https://gitlab.com/%s/%s/-/commit/%s" org repo hash

  let branch_url ~org ~repo ref =
    Fmt.str "https://gitlab.com/%s/%s/-/tree/%s" org repo ref

  let request_url ~org ~repo id =
    Fmt.str "https://gitlab.com/%s/%s/-/merge_requests/%s" org repo id

  let parse_ref r =
    match Astring.String.cuts ~sep:"/" r with
    | "refs" :: "heads" :: branch ->
        let branch = Astring.String.concat ~sep:"/" branch in
        `Branch branch
    | [ "refs"; "merge-requests"; id; "head" ] ->
        let id = int_of_string id in
        `Request id
    | _ -> `Unknown r
end)
