(** The monotonic per-store commit clock, hoisted to {!Tea_core.Clock} (roadmap
    step 8, D7). [Tea_persist.Clock] is a re-export alias so existing
    [Tea_persist.Clock] references — and [test/clock_test] — compile unchanged;
    the [include module type of] keeps the private [stamp] type equal across
    the two names, so a stamp minted through one is usable through the other. *)

include module type of struct
  include Tea_core.Clock
end
