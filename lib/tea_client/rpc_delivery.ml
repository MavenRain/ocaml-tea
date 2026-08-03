module Msg_seq = Tea_core.Prim.Msg_seq

type 'msg entry =
  { path : string
  ; body : string
  ; expect : (string, Tea_core.Cmd.http_failure) result -> 'msg
  }

(* [queue] is OLDEST-first, the reverse of {!Delivery}'s. That queue records on
   every keystroke and reads its whole backlog once per reconnect, so it pays
   at the read; this one records once per user-initiated call and reads the
   head on every send and every settle, so it pays at the write. [next] is the
   number the NEXT record will use and advances independently of what the queue
   still holds: an acknowledgement that empties the queue must not reset the
   numbering, or the server would read the tab's next call as a replay of one
   it already took. *)
type 'msg t =
  { tab : Tea_core.Prim.Tab_id.t
  ; next : Msg_seq.t
  ; queue : (Msg_seq.t * 'msg entry) list
  }

let v ~(tab : Tea_core.Prim.Tab_id.t) : 'msg t =
  { tab; next = Msg_seq.one; queue = [] }

let tab (t : 'msg t) : Tea_core.Prim.Tab_id.t = t.tab

let record (e : 'msg entry) (t : 'msg t) :
    ('msg t * (Msg_seq.t * 'msg entry)) option =
  Msg_seq.next t.next
  |> Option.map (fun (next : Msg_seq.t) ->
         ({ t with next; queue = t.queue @ [ (t.next, e) ] }, (t.next, e)))

(* The one sendable frame. Exposing the head rather than the queue is what
   makes one-in-flight structural: a sender cannot reach the second entry
   without acknowledging the first. *)
let head (t : 'msg t) : (Msg_seq.t * 'msg entry) option =
  match t.queue with
  | [] -> None
  | frame :: (_ : (Msg_seq.t * 'msg entry) list) -> Some frame

(* Drop exactly the acknowledged entry. A filter rather than a head-drop
   because the sequence number crossed the wire and came back: honouring it
   only when it names a live entry is what makes a late, repeated or alien
   acknowledgement a no-op instead of a silent eviction of the entry that
   happens to be at the head. *)
let ack (seq : Msg_seq.t) (t : 'msg t) : 'msg t =
  { t with
    queue =
      List.filter
        (fun ((n : Msg_seq.t), (_ : 'msg entry)) ->
          not (Int.equal (Msg_seq.compare n seq) 0))
        t.queue
  }

let rotate ~(tab : Tea_core.Prim.Tab_id.t) (t : 'msg t) : 'msg t = { t with tab }
let pending (t : 'msg t) : int = List.length t.queue
let is_empty (t : 'msg t) : bool = List.is_empty t.queue
