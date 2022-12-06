open Tyxml.Html

(* TODO: this is intended to be delete when the history page will be rewritten *)
let breadcrumbs steps page_title =
  let add (prefix, results) (label, link) =
    let prefix = Fmt.str "%s/%s" prefix link in
    let link = li [ a ~a:[ a_href prefix ] [ txt label ] ] in
    (prefix, link :: results)
  in
  let _, steps = List.fold_left add ("", []) steps in
  let steps = li [ b [ txt page_title ] ] :: steps in
  ol ~a:[ a_class [ "breadcrumbs" ] ] (List.rev steps)

module Make (M : Git_forge_intf.Forge) = struct
  let history_v ~org ~repo ~(history : Git_forge_intf.Client.Repo.ref_info list)
      =
    ul
      ~a:[ a_class [ "statuses" ] ]
      (history
      |> List.map @@ fun ref_info ->
         li
           ~a:
             [
               a_class
                 [
                   Build_status.class_name
                     ref_info.Git_forge_intf.Client.Repo.status;
                 ];
             ]
           [
             a
               ~a:
                 [
                   a_href
                     (Url.commit_url M.prefix ~org ~repo ~hash:ref_info.hash);
                 ]
               [ txt ref_info.hash ];
             div [ txt (Timestamps_durations.pp_timestamp ref_info.started_at) ];
             div [ txt (Common.duration ref_info.status ref_info.ran_for) ];
           ])

  let link_forge_refs ~org ~repo = function
    | [] -> txt "(not at the head of any monitored branch or merge request)"
    | refs ->
        let refs = List.map M.parse_ref refs in
        let f = function
          | `Branch branch ->
              span
                [
                  txt "branch ";
                  a
                    ~a:[ a_href (M.branch_url ~org ~repo branch) ]
                    [ txt branch ];
                ]
          | `Request id ->
              let id = string_of_int id in
              span
                [
                  txt M.request_abbrev;
                  a
                    ~a:[ a_href (M.request_url ~org ~repo id) ]
                    [ txt ("#" ^ id) ];
                ]
          | `Unknown s -> txt (Fmt.str "Bad ref format %S" s)
        in
        p
          ((txt "(for " :: Common.intersperse ~sep:(txt ", ") (List.map f refs))
          @ [ txt ")" ])

  let list ~org ~repo ~ref ~history =
    Template.instance
      [
        breadcrumbs [ (M.prefix, M.prefix); (org, org) ] repo;
        link_forge_refs ~org ~repo [ ref ];
        history_v ~org ~repo ~history;
      ]
end
