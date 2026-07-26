type channel =
  | Local_only
  | Shared

module Make
    (A : Tea_core.App.APP)
    (L : Tea_core.Local.LOCAL with type shared = A.model and type msg = A.msg) =
struct
  type state =
    { shared : A.model
    ; local : L.local
    }

  type step =
    { state : state
    ; cmd : A.msg Tea_core.Cmd.t
    ; channel : channel
    }

  let init : state * A.msg Tea_core.Cmd.t =
    let model, cmd = A.init in
    ({ shared = model; local = L.init }, cmd)

  (* Companion first, [A.update] only if it declines. The decline branch must
     stay UNEVALUATED when the companion claims the message: running
     [A.update] anyway would double-step the shared model and mint a CRDT dot
     for an edit the app never made. [Option.fold]'s [~none:] is a value and
     would do exactly that, so the dispatch goes through [Result.fold], whose
     two arms are both functions. *)
  let update (ctx : Tea_core.Crdt.Ctx.t) (msg : A.msg) (state : state) : step =
    L.update msg state.shared state.local
    |> Option.to_result ~none:()
    |> Result.fold
         ~ok:(fun (local, cmd) ->
           { state = { state with local }; cmd; channel = Local_only })
         ~error:(fun () ->
           let shared, cmd = A.update ctx msg state.shared in
           { state = { state with shared }; cmd; channel = Shared })

  let view (state : state) : A.msg Tea_core.Html.t = L.view state.shared state.local
end
