(** Pure commit-coalescing policy for chatty Msgs (R1, roadmap step 6).

    The store folds a run of messages into one commit by amending the branch
    head (same parents, new tree, relabelled) for as long as the policy keeps
    returning [Some _]. The policy is pure data, mirroring {!Merge_spec}:
    property-testable without Irmin, and an app that says nothing keeps the
    historical one-commit-per-Msg behaviour.

    A run ends on a policy [None], any head movement the coalescer did not
    mint itself (a plain commit, a merge, an undo), or an explicit seal —
    never on a wall-clock timer, so there is no debounce loss window. *)

type 'msg t =
  | Keep_all
      (** One commit per Msg — the historical behaviour; the coalescer is
          inert. *)
  | Fold_run of (last:'msg -> next:'msg -> 'msg option)
      (** [Some folded]: [next] continues the run — the head commit is
          amended and relabelled with [folded]. [None]: run boundary — [next]
          starts a fresh commit. *)
