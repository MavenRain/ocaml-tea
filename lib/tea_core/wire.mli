(** The client↔server live-view wire surface (roadmap step 3, DESIGN §7).

    Both tiers link this module, so neither the endpoint path nor the shape of
    a down-frame can drift between them — the same no-drift discipline as the
    shared [APP] itself. The payload {i encodings} still need no definition
    here: they are the app's own Repr-JSON codecs ({!Codec}).

    Both directions are sums: {!up} travels client to server, {!down} server to
    client. *)

val ws_path : string
(** The WebSocket endpoint every ocaml-tea server mounts and every live-view
    client dials. *)

(** What the client says on a live socket (roadmap step 10, D15).

    Up-frames used to be bare app-msg JSON — the client's only up-verb being
    "apply this message". They now carry a delivery header, so the server can
    tell a retry from a new edit. Without one there can be no retry at all: a
    message handed to a socket that then dies was never applied and was in no
    outbox, so it was lost silently; and a retry added without a way to
    recognise it double-applies, because state {i join} is idempotent while
    replaying an {i operation} is not (§7, D14).

    The header fields are primitives rather than newtypes on purpose. The
    client chooses both, so they are still untrusted here, and they become
    {!Prim.Tab_id.t} / {!Prim.Msg_seq.t} only after the server's own
    [of_string] / [of_int] accept them. A [Repr] witness for an abstract type
    is total on decode and would admit inhabitants the newtype's own
    constructor rejects — the validation has to happen where the frame lands,
    not in the codec. *)
type 'msg up =
  | Apply of
      { tab : string  (** the sending tab's id, {!Prim.Tab_id} grammar *)
      ; seq : int  (** that tab's message number, 1-based and gapless *)
      ; msg : 'msg
      }

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
    - [Ack seq] is {b cumulative} (roadmap step 10, D15): every message this
      socket's tab sent with a number at or below [seq] has been consumed by
      the server and will never be applied again. It is minted by the {i pump},
      never by the store watch, and it is sent for a duplicate exactly as for a
      fresh message — an unacknowledged duplicate is a replay loop that never
      terminates. A [Head] carries no delivery meaning at all: the watch fires
      for every writer on the branch (a form post, an undo, the {i other tab on
      the same cookie}), so a head can never stand in for an acknowledgement.

    A client that has not yet been told its replica is a client whose local
    dots are provisional — see {!Tea_client.Identity}. *)
type 'model down =
  | Hello of Crdt.Replica.t * 'model
  | Head of 'model
  | Ack of Prim.Msg_seq.t
