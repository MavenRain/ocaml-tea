(** The Shared-document server tier as a {i linkable} value (roadmap step 8,
    D12): the Dream-side wiring of {!Shared_doc_rpc} lives here rather than in
    [main.ml] so the test suite drives the SAME handler the binary serves.

    That matters for the mutating endpoint specifically: [test/csrf_test]
    asserts that a same-origin [/rpc/append_tag] really writes the store and
    that a cross-origin one really does not, and neither assertion means
    anything against a second hand-written copy of the handler. Same doctrine
    as {!Shared_doc_rpc} itself: one definition, linked by everyone. *)

(** The typed RPC dispatcher for this app's contract, gated per
    {!Tea_rpc.endpoint_kind}. Tier-independent, and stated once here rather than
    once per tier: the contract describes the wire, not where the model is
    kept. *)
module Rpc = Tea_server.Rpc (Shared_doc_rpc)

(** The RPC tier over {i any} store that can hold this app's model.

    A functor rather than a body written against the mem store, because roadmap
    step 15 put the same handler on two tiers: the mem binary answers
    [/rpc/append_tag] out of a store that dies with the process, and the durable
    binary answers it out of irmin-pack, where the [Mutating] endpoint's
    delivery floors ride a journal and so survive a restart. Only the durable
    one can state the exactly-once guarantee end to end - the guarantee is about
    what a restart preserves - so the pack tier is not an optional extra for
    this app but the tier the step is about.

    One definition serving both is the same doctrine that put this module in a
    library instead of in [main.ml]: a second hand-written copy of the handler
    would make every assertion about "the handler" ambiguous. *)
module Make_rpc
    (St : Tea_persist.Store_core.CORE
            with type model = Shared_doc_app.App.model
             and type msg = Shared_doc_app.App.msg) =
struct
  (** The framework's request tier over that store. What this handler needs from
      it is [step]: the D19 test-and-set commit path, the same one the browser
      and the form posts run. *)
  module Tier = Tea_server.Handlers (Shared_doc_app.App) (St)

  (** The rank-2 RPC handler over one repo. Request-free by contract (it cannot
      see the Dream session), so every endpoint speaks about the canonical
      branch: [History_count] counts its commits, [Append_tag] writes it.

      [Append_tag] goes through {!Tier.step} rather than poking the store, so
      the tag lands as one [Add_tag] Msg: an ordinary labelled commit in the
      canonical branch's event log, produced by the same [App.update] the
      browser and the form-post path run. An RPC that reached into the store by
      any other route would be a second write path with its own semantics.

      Two limits are worth stating plainly, because the obvious readings of "one
      commit on the canonical branch" are both wrong.

      - It does NOT reach live WebSocket peers. A live session watches the
        {i per-cookie} session branch that {!Tea_server.Handlers.accept_ws}
        resolves, and the canonical branch is not that branch, so no currently
        connected client sees an appended tag until something merges the two.
        Cross-branch propagation is the R3 auto-merge story, still deferred;
        today the effect of this endpoint is observable through [History_count]
        and a direct read of the canonical branch, which is what
        [test/csrf_test] does.
      - This endpoint is the first to put {i different} clients on one branch
        (every other write path is per-session, where the only racer is the same
        user), and until roadmap step 14 that made it R10's live instance: the
        framework's step was load, step, commit with no compare-and-set, so two
        concurrent appends each read the pre-append document and the second
        erased the first. The app carried its own [Lwt_mutex] across the
        read-modify-write to cover it.

        That lock is gone since D19. {!Tier.step} now reads through a witness
        token and commits by test-and-set, reconciling a concurrent write with
        the app's own merge instead of overwriting it, and the seam takes the
        token rather than a session, so this handler {i cannot} reach the
        last-write-wins door even by accident. The honest statement of the trade
        is that the guarantee moved rather than that a check replaced a lock:
        deleting the mutex used to leave [test/csrf_test] green (there is no Lwt
        yield between load and commit under [Irmin_mem] with a [Cmd.none] Msg,
        so the interleave was not reachable from here), so the lock pinned
        nothing. What pins the property now lives at the framework tier, in
        [test/contention_test]. *)
  let rpc_handler (repo : St.t) : Rpc.keyed_handler =
    let handle_keyed :
        type a b. (a, b) Shared_doc_rpc.t -> a -> (b * Tea_core.Prim.Store_water.t) Lwt.t =
     fun ep req ->
      match ep with
      (* The two [Read_only] arms answer [bottom]: they commit nothing, and the
         guarded route reads the water only on the keyed path, where a floor
         claiming a water no commit minted would outlive a boot filter it should
         not have survived. *)
      | Shared_doc_rpc.History_count ->
        Lwt.bind (St.main_session repo) St.history
        |> Lwt.map (fun (h : Tea_core.Prim.Commit_ref.t list) ->
               (List.length h, Tea_core.Prim.Store_water.bottom))
      | Shared_doc_rpc.Doc_stats ->
        Lwt.return (Shared_doc_rpc.stats_of req, Tea_core.Prim.Store_water.bottom)
      | Shared_doc_rpc.Append_tag ->
        Lwt.bind (St.main_session repo) (fun s ->
            Lwt.bind (Tier.step s (Shared_doc_app.App.Add_tag req)) (fun stepped ->
                Result.fold stepped
                  ~ok:(fun (o : Tier.step_outcome) ->
                    (* The COMMITTED model, which since D19 is the reconciled one
                       under contention rather than this request's own
                       transition, so the count reported back is the count the
                       branch actually holds. *)
                    Lwt.return (List.length (Shared_doc_app.App.tags_of o.model), o.water))
                  ~error:(fun (Tier.Loop.Fuel_exhausted : Tier.Loop.err) ->
                    (* Dead as written - [Add_tag] emits [Cmd.none], so the
                       loop settles in one step - but total, and honest if a
                       future [Add_tag] ever grows a Cmd tail: [step_with]
                       returns this error WITHOUT committing, so re-reading
                       the branch reports what actually landed instead of
                       inventing a count. The reply type cannot distinguish
                       that from an idempotent re-add, which is a recorded
                       residual: a fallible endpoint should declare
                       ['resp = (_, _) result] (DESIGN section 8).

                       The water is [bottom] here for the reason the arm exists:
                       nothing was committed, so this delivery has no witness to
                       claim. Taken still means attempted, once per journal
                       lifetime — the D16 "taken, never applied" sentence. *)
                    Lwt.map
                      (fun m ->
                        ( List.length (Shared_doc_app.App.tags_of m)
                        , Tea_core.Prim.Store_water.bottom ))
                      (St.load s))))
    in
    { Rpc.handle_keyed }
end

(** The mem-backed Dream tier for this app: the model dies with the process and
    the delivery floors die with it, so this tier's keyed channel is
    exactly-once within one process life and no further (D16, D20). *)
module Server = Tea_server.Make (Shared_doc_app.App)

module Mem_rpc = Make_rpc (Server.Store)

(** The mem tier's handler under its historical name, so the suites that already
    drive it ([test/csrf_test], [test/rpc_test]) name it unchanged. *)
let rpc_handler : Server.Store.t -> Rpc.keyed_handler = Mem_rpc.rpc_handler

(** The full request pipeline the binary runs and the tests drive: SSR + form
    posts at [/], the live WebSocket, the three typed RPC routes, and - when a
    compiled client bundle is on disk - the browser client at [/app].

    [?once] is the exactly-once channel this app's [Mutating] endpoint runs
    under (roadmap step 15). It defaults to the mem tier's own
    {!Tea_server.Rpc.once_mem} over the canonical replica, which is the branch
    [Append_tag] commits on and therefore the single-branch contract this app
    declares. A caller supplies its own to compose a durable guard over a
    journal, or to hold the recording sink a restart scenario reads back.

    [?on_taken] is {!Tea_server.Rpc.routes_once}'s tests-only seam, threaded
    through for the same reason [?once] is: the window between a [Fresh]
    verdict and the handler call is not reachable from outside the pipeline,
    and a suite that rebuilt the routes to reach it would be asserting about
    its own copy of the wiring rather than about the handler this module
    exports. It defaults to a no-op, so the binary's pipeline is unchanged. *)
let handler ?client_dir ?once ?on_taken (repo : Server.Store.t) : Dream.handler =
  let once =
    Option.fold once
      ~none:(fun () -> Rpc.once_mem ~floor_replica:Server.Store.main_replica ())
      ~some:(fun (o : Rpc.once) () -> o)
      ()
  in
  Server.handler ?client_dir ~rpc:(Rpc.routes_once ?on_taken once (rpc_handler repo)) repo

(** The durable tier: the same app and the same RPC handler over irmin-pack.

    Here rather than in [main.ml] for the reason this module exists at all, and
    the reason is sharper for the keyed channel than for anything before it:
    what step 15 adds is not a route but {i which guard a route runs under}, and
    a copy of that wiring in the entry-point binary would be reachable only
    through a spawned process. The browser harness does spawn one - that is
    B10's whole point - but a wiring that only a subprocess can reach is a
    wiring no unit test can pin. *)
module Pack = struct
  module Server = Tea_server_pack.Make_pack (Shared_doc_app.App)
  module Rpc_tier = Make_rpc (Server.Store)

  (** The [?rpc_once] builder {!Tea_server_pack.Make_pack.serve_pack} applies to
      the store it opened and the channel it built over the journal it opened.

      It mounts every route of the contract, not only the [Mutating] one:
      {!Rpc.routes_once} gates each endpoint by its own {!Tea_rpc.endpoint_kind},
      so the two [Read_only] endpoints are mounted unkeyed by the same call and
      an app never has to pass its handler twice. *)
  let rpc_once (repo : Server.Store.t) (o : Rpc.once) : Dream.route list =
    Rpc.routes_once o (Rpc_tier.rpc_handler repo)

  (** The durable binary's blocking entry point: the pack store at [root], the
      guard journals at its siblings, the durable session secret, and this app's
      RPC contract on the keyed channel. *)
  let serve ?client_dir ?port ~(root : Tea_server_pack.Root.t) () : unit =
    Server.serve_pack ?port ?client_dir ~root ~rpc_once ()
end
