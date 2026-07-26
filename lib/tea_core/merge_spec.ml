type 'model t =
  | Last_write_wins
  | Three_way of
      (ancestor:'model option -> ours:'model -> theirs:'model -> ('model, string) result)
  | Crdt_join of ('model -> 'model -> 'model)

let crdt_join (join : 'model -> 'model -> 'model) : 'model t = Crdt_join join
