let gitlab_branch_url ~org ~repo ref =
  Fmt.str "https://gitlab.com/%s/%s/-/tree/%s" org repo ref

let gitlab_mr_url ~org ~repo id =
  Fmt.str "https://gitlab.com/%s/%s/-/merge_requests/%s" org repo id

let gitlab_commit_url ~org ~repo ~hash =
  Printf.sprintf "https://gitlab.com/%s/%s/-/commit/%s" org repo hash

include Git_forge.Make (struct
  let prefix = "gitlab"
  let request_abbrev = "MR"
  let commit_url = gitlab_commit_url
  let branch_url = gitlab_branch_url
  let request_url = gitlab_mr_url

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
