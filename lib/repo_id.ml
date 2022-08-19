type t = { owner : string; name : string }

let pp f { owner; name } = Fmt.pf f "%s/%s" owner name
let compare = compare
