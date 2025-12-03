let ( >>!= ) = Lwt_result.Infix.( >>= )

type source =
  | Opam of { version : string; opam_repo_commit : string option }
  | Vendored of { path : string }
[@@deriving yojson, eq, ord]

let pp_source f = function
  | Opam { version; _ } -> Fmt.pf f "version %s (from opam)" version
  | Vendored { path } -> Fmt.pf f "vendored at %s" path

let ocamlformat_version_from_string =
  let re =
    Re.(
      seq
        [
          start;
          rep space;
          str "version";
          rep space;
          char '=';
          rep space;
          group (rep1 @@ diff graph (set "#"));
          rep space;
          eol;
        ]
      |> compile)
  in
  fun path ->
    Re.exec_opt re path |> function
    | Some g -> Some (Re.Group.get g 1)
    | None -> None

let ocamlformat_version_from_file ~root job path =
  let ( let* ) = Lwt.Infix.( >>= ) in
  if not (Sys.file_exists path) then
    let () = Current.Job.log job "No .ocamlformat file found" in
    Lwt.return (Ok None)
  else
    let* lines =
      Lwt_io.with_file ~mode:Lwt_io.input path (fun channel ->
          Lwt_io.read_lines channel |> Lwt_stream.to_list)
    in
    let versions = List.filter_map ocamlformat_version_from_string lines in
    match versions with
    | [ v ] ->
        let () =
          Current.Job.log job "Found OCamlFormat version '%s' in dotfile" v
        in
        Lwt.return (Ok (Some v))
    | [] -> (
        (* Search for any .ocamlformat-enable files as a disabled ocamlformat would
           still format things if a ocamlformat-enable file is present *)
        let cmd =
          ( "",
            [|
              "git"; "ls-files"; "**/.ocamlformat-enable"; ".ocamlformat-enable";
            |] )
        in
        Current.Process.check_output ~cwd:root ~cancellable:true ~job cmd
        >>!= function
        | "" ->
            if List.mem "disable" lines then Lwt.return (Ok None)
            else
              Lwt.return
                (Error (`Msg "Missing 'version=' line in .ocamlformat"))
        | _ ->
            Lwt.return
              (Error
                 (`Msg
                    "Missing 'version=' line in .ocamlformat (disabled, but \
                     some .ocamlformat-enable files are present)")))
    | _ :: _ :: _ ->
        Lwt.return (Error (`Msg "Multiple 'version=' lines in .ocamlformat"))

let get_ocamlformat_source ~opam_files ~version ~find_opam_repo_commit =
  let proj_is_ocamlformat p =
    String.equal (Filename.basename p) "ocamlformat.opam"
  in
  match List.find_opt proj_is_ocamlformat opam_files with
  | Some opam_file ->
      let path = Filename.dirname opam_file in
      Lwt_result.return (Some (Vendored { path }), None)
  | None -> (
      match version with
      | None -> Lwt_result.return (None, None)
      | Some version ->
          find_opam_repo_commit version >>!= fun (x, ocamlformat_selection) ->
          Lwt_result.return
            ( Some (Opam { version; opam_repo_commit = Some x }),
              Some ocamlformat_selection ))
