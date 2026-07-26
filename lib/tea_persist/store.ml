(** The versioned model store: the "Irmin instead of SQLite" core (T1).

    For an [APP], the model lives on a per-session Irmin branch at path
    [\["model"\]]. Every TEA step is exactly one commit whose message is the
    serialized Msg, so the commit log {i is} the event log: undo/redo and
    time-travel are [Head]/parents operations, not bespoke features. Concurrent
    sessions reconcile through Irmin's tree merge, which dispatches to the
    [Contents.merge] lifted from the app's {!Tea_core.Merge_spec}.

    Since roadmap step 6 the whole body lives in {!Store_core} (backend-
    generic); this module is the in-memory shim ([irmin.mem]) used by tests
    and the Dream tier today. The durable pack shim with GC is
    [Tea_persist_pack.Store_pack]. *)

module Make (A : Tea_core.App.APP) = struct
  module Contents = Store_core.Contents (A)
  module Mem = Irmin_mem.KV.Make (Contents)
  include Store_core.Make (A) (Mem)

  (** [?now] injects the wall source for the commit clock — tests freeze it;
      the default reads the system clock. *)
  let create ?(now = default_now) () : t Lwt.t =
    Lwt.bind (S.Repo.v (Irmin_mem.config ())) (v ~now)
end
