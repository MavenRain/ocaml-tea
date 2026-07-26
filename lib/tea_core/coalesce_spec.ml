type 'msg t =
  | Keep_all
  | Fold_run of (last:'msg -> next:'msg -> 'msg option)
