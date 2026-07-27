let ws_path = "/ws"

type 'msg up =
  | Apply of
      { tab : string
      ; seq : int
      ; msg : 'msg
      }

type 'model down =
  | Hello of Crdt.Replica.t * 'model
  | Head of 'model
  | Ack of Prim.Msg_seq.t
