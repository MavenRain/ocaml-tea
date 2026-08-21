(** Native fake of the step-25 [Local_store.BACKEND] (D25). Honest on exactly
    the two properties a lenient fake would wave through: every completion is
    delivered via an explicit {!tick} - never synchronously from the issuing
    call - and a transaction only accepts requests issued in the turn it was
    opened (the mirrored auto-commit boundary). Both honesty properties are
    themselves pinned by checks in [local_store_test] and killed by mutation,
    so the fake cannot silently drift lenient. *)

type t

val create : unit -> t
(** A fresh empty store. Also registers itself as the instance the next
    [open_db] answers with: [BACKEND.open_db] mints the handle, so the fake
    routes it to the most recently created instance. *)

val inject : t -> Tea_client.Local_store.idb_error -> unit
(** The NEXT operation on [t] fails with this error, delivered async via
    {!tick}. [Blocked] is delivered through [on_blocked] and is then followed
    by a queued success, which is exactly the double-fire the Flow boot's
    once-guard must absorb. Consumed by one operation. *)

val tick : t -> bool
(** Deliver ONE queued completion; [false] when idle. Every call advances the
    turn counter, delivered or not, so a transaction opened before any [tick]
    is dead after it. *)

val drain : t -> unit
(** [tick] until idle. *)

val rows : t -> (string * string) list
(** Store contents, key to value, newest put first. *)

val version_change : t -> unit
(** Fire the last opened connection's [on_versionchange] (async, via the
    queue): the second-tab upgrade signal, driven natively. *)

include Tea_client.Local_store.BACKEND with type db = t

(** The low-level honesty surface, driven directly by the invariant checks. *)

type txn

val txn_open : t -> txn

val txn_request :
  t ->
  txn ->
  op:[ `Clear | `Put of string * string ] ->
  (unit, Tea_client.Local_store.idb_error) result
(** [Error (Other "TransactionInactiveError")] once any {!tick} has run since
    {!txn_open} - the mirrored auto-commit boundary. [replace_all] is
    implemented over ONE {!txn_open} plus two same-turn {!txn_request}s, so
    the high-level path exercises the same discipline the jsoo shell
    promises. *)
