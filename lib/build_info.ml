type t = { label : string; variant : Variant.t option }
(** [variant] will be [None] for utility or linting jobs without a platform, and
    [Some v] for jobs building on a particular platform [v]. *)

let of_spec = function
  | Spec.{ label; variant; ty = _ } -> { label; variant = Some variant }

let of_label label = { label; variant = None }

(** Check whether a variant is considered experimental.

    If it is experimental we allow those builds to fail without failing the
    overall build for a commit. *)
let experimental_variant s =
  if
    Astring.String.(
      is_prefix ~affix:Variant.lower_bound_label s.label
      || is_prefix ~affix:Variant.opam_label s.label)
  then true
  else
    match s.variant with
    | None -> false
    | Some v ->
       (* Opam 2.2 is experimental version for now. *)
       Opam_version.equal (Variant.opam_version v) `V2_2
       || Astring.String.equal "freebsd" (Variant.distro v)
       || Astring.String.equal "macos-homebrew" (Variant.distro v)
       || Ocaml_version.(equal (v 5 1 ~patch:0)) (Variant.ocaml_version v)
       || Ocaml_version.(equal (v 5 1 ~patch:0 ~prerelease:"alpha1"))
            (Variant.ocaml_version v)
       || Ocaml_version.(equal (v 5 1 ~patch:0 ~prerelease:"alpha2"))
            (Variant.ocaml_version v)
       || Ocaml_version.(equal (v 5 1 ~patch:0 ~prerelease:"beta1"))
            (Variant.ocaml_version v)

(** Like [experimental_variant], but takes strings for when a [build_info]
    record is unavailable.

    Prefer to use [experimental_variant] when possible, as this function can
    potentially give false positives. *)
let experimental_variant_str s =
  Astring.String.(
    is_infix ~affix:"opam-2.2" s
    || is_prefix ~affix:Variant.lower_bound_label s
    || is_prefix ~affix:Variant.opam_label s
    || is_prefix ~affix:"freebsd" s
    || is_prefix ~affix:"macos-homebrew" s
    || is_infix ~affix:"-5.1" s
    || is_infix ~affix:"-5.1~alpha1" s
    || is_infix ~affix:"-5.1.0~alpha1" s
    || is_infix ~affix:"-5.1~alpha2" s
    || is_infix ~affix:"-5.1.0~alpha2" s
    || is_infix ~affix:"-5.1.0~beta1" s)
