open Lwt.Infix

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

let ocamlformat_version_from_file job path =
  let ( let+ ) = Lwt.Infix.( >|= ) in
  if not (Sys.file_exists path) then
    let () = Current.Job.log job "No .ocamlformat file found" in
    Lwt.return (Ok None)
  else
    let+ versions = Lwt_io.with_file ~mode:Lwt_io.input path (fun channel ->
        Lwt_io.read_lines channel
        |> Lwt_stream.filter_map ocamlformat_version_from_string
        |> Lwt_stream.to_list
      )
    in
    match versions with
    | [ v ] ->
        let () =
          Current.Job.log job "Found OCamlformat version '%s' in dotfile" v
        in
        Ok (Some v)
    | _ -> Error (`Msg "Unable to parse .ocamlformat file")

let get_ocamlformat_version job root =
  Fpath.(to_string (root / ".ocamlformat")) |> ocamlformat_version_from_file job
  >|= function
  | Ok result -> result
  | Error (`Msg e) -> failwith e
