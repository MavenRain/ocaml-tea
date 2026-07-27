let ws_path = "/ws"

type 'model down =
  | Hello of Crdt.Replica.t * 'model
  | Head of 'model
