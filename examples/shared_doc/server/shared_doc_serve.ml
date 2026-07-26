(** The Shared-document server tier as a {i linkable} value (roadmap step 8,
    D12): the Dream-side wiring of {!Shared_doc_rpc} lives here rather than in
    [main.ml] so the test suite drives the SAME handler the binary serves.

    That matters for the mutating endpoint specifically: [test/csrf_test]
    asserts that a same-origin [/rpc/append_tag] really writes the store and
    that a cross-origin one really does not, and neither assertion means
    anything against a second hand-written copy of the handler. Same doctrine
    as {!Shared_doc_rpc} itself: one definition, linked by everyone. *)

(** The mem-backed Dream tier for this app. *)
module Server = Tea_server.Make (Shared_doc_app.App)

(** The typed RPC dispatcher for this app's contract, gated per
    {!Tea_rpc.endpoint_kind}. *)
module Rpc = Tea_server.Rpc (Shared_doc_rpc)

(** The rank-2 RPC handler over one repo. Request-free by contract (it cannot
    see the Dream session), so every endpoint speaks about the canonical
    branch: [History_count] counts its commits, [Append_tag] writes it.

    [Append_tag] goes through {!Server.step} rather than poking the store, so
    the tag lands as one [Add_tag] Msg: an ordinary labelled commit in the
    canonical branch's event log, produced by the same [App.update] the browser
    and the form-post path run. An RPC that reached into the store by any other
    route would be a second write path with its own semantics.

    Two limits are worth stating plainly, because the obvious readings of "one
    commit on the canonical branch" are both wrong.

    - It does NOT reach live WebSocket peers. A live session watches the
      {i per-cookie} session branch that {!Tea_server.Handlers.accept_ws}
      resolves, and the canonical branch is not that branch, so no currently
      connected client sees an appended tag until something merges the two.
      Cross-branch propagation is the R3 auto-merge story, still deferred; today
      the effect of this endpoint is observable through [History_count] and a
      direct read of the canonical branch, which is what [test/csrf_test] does.
    - {!Server.step} is load, step, commit with no compare-and-set
      ([Store.commit] is a plain [set], last write wins), so two concurrent
      appends could each read the pre-append document and the second could
      erase the first. Every other write path is per-session, where the only
      racer is the same user; this endpoint is the first to put {i different}
      clients on one branch. Until the framework's step grows a CAS retry, the
      app serializes its own shared-branch mutations through [mutate_lock]
      below - correct within a process, which is what this single-process
      example is. *)
let rpc_handler (repo : Server.Store.t) : Rpc.handler =
  (* One lock per handler, held across load-step-commit so the read-modify-write
     above cannot interleave with itself. Nothing else writes the canonical
     branch in this example. *)
  let mutate_lock = Lwt_mutex.create () in
  let handle : type a b. (a, b) Shared_doc_rpc.t -> a -> b Lwt.t =
   fun ep req ->
    match ep with
    | Shared_doc_rpc.History_count ->
      Lwt.bind (Server.Store.main_session repo) Server.Store.history
      |> Lwt.map List.length
    | Shared_doc_rpc.Doc_stats -> Lwt.return (Shared_doc_rpc.stats_of req)
    | Shared_doc_rpc.Append_tag ->
      Lwt_mutex.with_lock mutate_lock (fun () ->
          Lwt.bind (Server.Store.main_session repo) (fun s ->
              Lwt.bind (Server.step s (Shared_doc_app.App.Add_tag req))
                (fun stepped ->
                  Result.fold stepped
                    ~ok:(fun (o : Server.step_outcome) ->
                      Lwt.return (List.length (Shared_doc_app.App.tags_of o.model)))
                    ~error:(fun (Server.Loop.Fuel_exhausted : Server.Loop.err) ->
                      (* Dead as written - [Add_tag] emits [Cmd.none], so the
                         loop settles in one step - but total, and honest if a
                         future [Add_tag] ever grows a Cmd tail: [step_with]
                         returns this error WITHOUT committing, so re-reading
                         the branch reports what actually landed instead of
                         inventing a count. The reply type cannot distinguish
                         that from an idempotent re-add, which is a recorded
                         residual: a fallible endpoint should declare
                         ['resp = (_, _) result] (DESIGN section 8). *)
                      Lwt.map
                        (fun m -> List.length (Shared_doc_app.App.tags_of m))
                        (Server.Store.load s)))))
  in
  { Rpc.handle }

(** The full request pipeline the binary runs and the tests drive: SSR + form
    posts at [/], the live WebSocket, the three typed RPC routes, and - when a
    compiled client bundle is on disk - the browser client at [/app]. *)
let handler ?client_dir (repo : Server.Store.t) : Dream.handler =
  Server.handler ?client_dir ~rpc:(Rpc.routes (rpc_handler repo)) repo
