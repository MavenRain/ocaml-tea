(** Web Locks shell (step 25, D25): single-writer election. *)

val supported : unit -> bool
(** [navigator.locks] presence. Absent: the caller never contends and never
    adopts (the memory-only degrade, F12). *)

val acquire : name:string -> granted:(bool -> unit) -> unit
(** [navigator.locks.request name {mode="exclusive"; ifAvailable=true} cb]:
    [granted true] means this page is the sole writer and the lock is held
    for the page's remaining life (the callback returns a promise that never
    resolves); [granted false] means another live tab holds it. [granted]
    fires exactly once, from a later task. Never raises. *)
