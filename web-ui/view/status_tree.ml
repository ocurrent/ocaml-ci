type key = string

type 'a tree = Leaf of 'a | Branch of key * 'a t
and 'a t = 'a tree list

let rec add k x ts =
  match (k, ts) with
  | [], ts -> ts @ [ Leaf x ]
  | k :: ks, [] -> [ Branch (k, add ks x []) ]
  | _ :: _, (Leaf _ as t) :: ts -> t :: add k x ts
  | k :: ks, Branch (k', t) :: ts when Astring.String.equal k k' ->
      Branch (k, add ks x t) :: ts
  | _ :: _, (Branch _ as t) :: ts -> t :: add k x ts
