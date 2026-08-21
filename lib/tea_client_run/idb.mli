(** jsoo IndexedDB shell (step 25, D25): the ONLY module that touches
    IndexedDB. Hand-rolled over [Ojs] per the repo's minimal-lift precedent
    (no [Js_browser] wrapper exists; [Event.kind]'s [Blocked] tag is a
    DOM-generic coincidence and must not be reused). Satisfies
    {!Tea_client.Local_store.BACKEND} with [db = Ojs.t]. Zero protocol
    decisions live here; every failure becomes data via
    {!Tea_client.Local_store.classify}, and no exception crosses the FFI (a
    synchronous throw is absorbed into the error arm). *)

val supported : unit -> bool
(** [window.indexedDB] presence, checked before any call (the [Unsupported]
    arm). *)

include Tea_client.Local_store.BACKEND with type db = Ojs.t
