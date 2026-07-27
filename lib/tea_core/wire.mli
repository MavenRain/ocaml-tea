(** The client↔server live-view wire surface (roadmap step 3, DESIGN §7).

    Both tiers link this module, so neither the endpoint path nor the shape of
    a down-frame can drift between them — the same no-drift discipline as the
    shared [APP] itself. The payload {i encodings} still need no definition
    here: they are the app's own Repr-JSON codecs ({!Codec}).

    ['msg] frames travel up the socket, unwrapped: the client's only up-verb is
    "apply this message". Down-frames are the {!down} sum below. *)

val ws_path : string
(** The WebSocket endpoint every ocaml-tea server mounts and every live-view
    client dials. *)

(** What the server says on a live socket (roadmap step 9, D14).

    - [Hello (replica, head)] opens every session: the {!Crdt.Replica} the
      server applies this session's messages under, plus the branch head as of
      that moment. The client mints its {i own} optimistic dots under the
      announced replica, so one user intent is one replica slot on both tiers
      and a [Pn_counter] join becomes a per-slot [max] instead of a sum — the
      D14 double count. The head rides along so adopting an identity and
      adopting the truth it belongs to is a single, atomic frame: there is no
      window in which the client holds the new replica id and a model still
      carrying dots minted under the old one.
    - [Head model] is every subsequent push: the committed head after a commit
      on this session's branch.

    A client that has not yet been told its replica is a client whose local
    dots are provisional — see {!Tea_client.Identity}. *)
type 'model down =
  | Hello of Crdt.Replica.t * 'model
  | Head of 'model
