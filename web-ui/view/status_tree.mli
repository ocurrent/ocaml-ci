type key = string

type 'a tree = Leaf of 'a | Branch of key * 'a t
and 'a t = 'a tree list

val add : key list -> 'a -> 'a t -> 'a t