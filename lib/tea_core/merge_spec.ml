type 'model t =
  | Last_write_wins
  | Three_way of
      (ancestor:'model option -> ours:'model -> theirs:'model -> ('model, string) result)
