open Lwt.Infix

type 'a t = {
  mutable current :
    [ `Idle
    | `Activating of unit Lwt.t (* Promise resolves after moving to [`Active] *)
    | `Active of string * 'a
    | `Draining of
      unit Lwt.t * unit Lwt_condition.t
      (* Promise resolves after moving back to [`Active] *) ];
  mutable users : int; (* Zero unless active or draining *)
  create : string -> 'a Lwt.t;
  dispose : 'a -> unit Lwt.t;
}

let activate t epoch ~ready ~set_ready =
  t.current <- `Activating ready;
  t.create epoch >|= fun v ->
  t.current <- `Active (epoch, v);
  Lwt.wakeup_later set_ready ()

let rec with_epoch t epoch fn =
  match t.current with
  | `Active (current_epoch, v) when current_epoch = epoch ->
      t.users <- t.users + 1;
      Lwt.finalize
        (fun () -> fn v)
        (fun () ->
          t.users <- t.users - 1;
          (match t.current with
          | `Active _ -> ()
          | `Draining (_, cond) ->
              if t.users = 0 then Lwt_condition.broadcast cond ()
          | `Idle | `Activating _ -> assert false);
          Lwt.return_unit)
  | `Active (_, old_v) ->
      let cond = Lwt_condition.create () in
      let ready, set_ready = Lwt.wait () in
      t.current <- `Draining (ready, cond);
      (* After this point, no new users can start. *)
      let rec drain () =
        if t.users = 0 then Lwt.return_unit
        else Lwt_condition.wait cond >>= drain
      in
      drain () >>= fun () ->
      t.dispose old_v >>= fun () ->
      activate t epoch ~ready ~set_ready >>= fun () -> with_epoch t epoch fn
  | `Draining (ready, _) | `Activating ready ->
      ready >>= fun () -> with_epoch t epoch fn
  | `Idle ->
      let ready, set_ready = Lwt.wait () in
      activate t epoch ~ready ~set_ready >>= fun () -> with_epoch t epoch fn

let v ~create ~dispose () = { current = `Idle; users = 0; create; dispose }
