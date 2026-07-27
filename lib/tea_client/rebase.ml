let reconcile (policy : 'model Tea_core.Merge_spec.t) ~(local : 'model)
    ~(incoming : 'model) : 'model =
  match policy with
  | Tea_core.Merge_spec.Crdt_join join -> join local incoming
  | Tea_core.Merge_spec.Three_way f ->
    (* No ancestor: the tab does not keep the head it last synced from, and
       inventing one (say, [incoming]) would make the merge report "ours did
       not change" for every local edit. [None] is the truthful input. *)
    f ~ancestor:None ~ours:local ~theirs:incoming
    |> Result.value ~default:incoming
  | Tea_core.Merge_spec.Last_write_wins -> incoming

type 'model absorbed =
  | Resync of Tea_core.Crdt.Replica.t * 'model
  | Rebased of 'model
  | Acked of Tea_core.Prim.Msg_seq.t

let absorb (policy : 'model Tea_core.Merge_spec.t) ~(local : 'model option)
    (down : 'model Tea_core.Wire.down) : 'model absorbed =
  match down with
  (* A [Hello] is a (re)connect: adopt the announced identity, and take the
     head it came with as-is rather than folding the local model into it. That
     local model predicts a session this tab may have fallen out of step with,
     and every locally-born edit the server has not applied yet is still in the
     delivery queue, unacknowledged, and replayed the moment the link is up —
     so resyncing to the truth loses no intent, only a stale prediction.

     Before D15 that sentence was a hope: a message already handed to the dead
     socket was in no queue at all, and this line discarded it. *)
  | Tea_core.Wire.Hello (replica, head) -> Resync (replica, head)
  | Tea_core.Wire.Head incoming ->
    Rebased
      (Option.fold ~none:incoming
         ~some:(fun local -> reconcile policy ~local ~incoming)
         local)
  | Tea_core.Wire.Ack seq -> Acked seq
