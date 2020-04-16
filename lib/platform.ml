type t = {
  label : string;
  builder : Builder.t;
  variant : string;
}

let pp f t = Fmt.string f t.label
let compare a b = compare a.label b.label
