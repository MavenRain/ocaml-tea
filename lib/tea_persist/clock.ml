(* Re-export of the clock, hoisted to [tea_core] (roadmap step 8, D7) so the
   js-pure app/CRDT layer can mint stamps without depending on persistence.
   This is an ALIAS, not a copy: [Tea_persist.Clock] and [Tea_core.Clock] are
   the SAME module, so there is a single clock instance and the private [stamp]
   type is shared across both layers. *)
include Tea_core.Clock
