module Outbox = struct
  type 'msg t = 'msg list

  let empty : 'msg t = []
  let buffer (msg : 'msg) (o : 'msg t) : 'msg t = msg :: o
  let drain (o : 'msg t) : 'msg list * 'msg t = (List.rev o, empty)
  let pending (o : 'msg t) : int = List.length o
  let is_empty (o : 'msg t) : bool = List.is_empty o
end

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

let absorb (policy : 'model Tea_core.Merge_spec.t) ~(local : 'model option)
    (down : 'model Tea_core.Wire.down) : 'model * Tea_core.Crdt.Replica.t option =
  match down with
  (* A [Hello] is a (re)connect: adopt the announced identity, and take the
     head it came with as-is rather than folding the local model into it. That
     local model predicts a session this tab may have fallen out of step with,
     and every locally-born edit the server has not applied yet is either in
     the outbox (replayed immediately after this) or already in flight — so
     resyncing to the truth loses no intent, only a stale prediction. *)
  | Tea_core.Wire.Hello (replica, head) -> (head, Some replica)
  | Tea_core.Wire.Head incoming ->
    ( Option.fold ~none:incoming
        ~some:(fun local -> reconcile policy ~local ~incoming)
        local
    , None )
