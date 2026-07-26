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
