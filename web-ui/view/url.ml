let org_url prefix ~org = Printf.sprintf "/%s/%s" prefix org
let repo_url prefix ~org ~repo = Printf.sprintf "/%s/%s/%s" prefix org repo

let commit_url prefix ~org ~repo ~hash =
  assert (String.length hash > 10);
  Printf.sprintf "/%s/%s/%s/commit/%s" prefix org repo hash

let job_url prefix ~org ~repo ~hash variant =
  assert (String.length hash > 10);
  Printf.sprintf "/%s/%s/%s/commit/%s/variant/%s" prefix org repo hash variant
