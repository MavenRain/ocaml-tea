(** See reaper.mli. Pure Lwt on purpose: the timer is injected, so this
    module never links Lwt_unix and a native test never sleeps. *)

module Cadence = struct
  type t = float

  let of_seconds (s : float) : t option = if s > 0. then Some s else None
  let to_seconds (t : t) : float = t
end

type spec =
  { ttl : Tea_core.Prim.Ttl.t
  ; every : Cadence.t
  }

let loop ~(sweep : now:int64 -> int Lwt.t) ~(now : unit -> int64)
    ~(timer : Cadence.t -> unit Lwt.t) ~(every : Cadence.t)
    ~(stop : unit Lwt.t) : unit Lwt.t =
  let open Lwt.Syntax in
  (* [choose], never [pick]: pick cancels the losing branch, and a cancelled
     sweep could stop between a guard tombstone and its branch removal (the
     live_session pump ruling). The entry check makes an already-resolved
     [stop] resolve the loop without minting a timer, and keeps the
     post-sweep round from minting one either. *)
  let rec go () : unit Lwt.t =
    if not (Lwt.is_sleeping stop) then Lwt.return_unit
    else
      let* () = Lwt.choose [ timer every; stop ] in
      if Lwt.is_sleeping stop then
        (* The sweep is fenced: nothing on its path is exception-free by
           contract (irmin raises), and an unfenced rejection would end
           every future sweep silently AND reject this promise - which on
           the pack tier poisons the [Lwt.join] with [Dream.serve] and
           skips the ordered teardown behind [Lwt_main.run]. A rejected
           sweep is one stderr line and a zero round; the loop's own
           promise never rejects. *)
        let* (_ : int) =
          Lwt.catch
            (fun () -> sweep ~now:(now ()))
            (fun (exn : exn) ->
              Printf.eprintf
                "tea_server: reaper: sweep failed (%s); the loop continues at the next tick\n%!"
                (Printexc.to_string exn);
              Lwt.return 0)
        in
        go ()
      else Lwt.return_unit
  in
  go ()
