module Msg_seq = Tea_core.Prim.Msg_seq

(* [queue] is newest-first so recording is O(1) — it runs on every keystroke of
   an editing session — and {!unacked} pays the reversal, which runs once per
   reconnect. [next] is the number the NEXT record will use, so it advances
   independently of what the queue still holds: an acknowledgement that empties
   the queue must not reset the numbering, or the server would read the tab's
   next edit as a replay of one it already consumed. *)
type 'msg t =
  { tab : Tea_core.Prim.Tab_id.t
  ; next : Msg_seq.t
  ; queue : (Msg_seq.t * 'msg) list
  }

let v ~(tab : Tea_core.Prim.Tab_id.t) : 'msg t =
  { tab; next = Msg_seq.one; queue = [] }

let tab (t : 'msg t) : Tea_core.Prim.Tab_id.t = t.tab

let record (msg : 'msg) (t : 'msg t) : ('msg t * (Msg_seq.t * 'msg)) option =
  Msg_seq.next t.next
  |> Option.map (fun (next : Msg_seq.t) ->
         ({ t with next; queue = (t.next, msg) :: t.queue }, (t.next, msg)))

let ack (seq : Msg_seq.t) (t : 'msg t) : 'msg t =
  { t with
    queue = List.filter (fun ((n : Msg_seq.t), (_ : 'msg)) -> Msg_seq.compare n seq > 0) t.queue
  }

let unacked (t : 'msg t) : (Msg_seq.t * 'msg) list = List.rev t.queue
let pending (t : 'msg t) : int = List.length t.queue
let is_empty (t : 'msg t) : bool = List.is_empty t.queue
