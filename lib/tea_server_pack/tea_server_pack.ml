(** The durable pack-backed Dream tier (roadmap step 8, D2).

    [Make_pack (A)] instantiates the {i same} {!Tea_server.Handlers} bodies as
    the mem tier, but over {!Tea_persist_pack.Store_pack} rather than the
    in-memory store, and adds one Origin-gated endpoint:
    [POST /admin/checkpoint], which squashes [main], links the checkpoint onto
    the durable retention spine, and GCs to the retention anchor.

    Living in its own library is deliberate: it keeps irmin-pack (and Unix) out
    of [tea_server]'s dependency closure, so the js_of_ocaml client tier never
    drags a native-only backend into its link. *)

module Root = Tea_persist_pack.Store_pack.Root
module Guard_file = Guard_file
module Guard_sink = Tea_server.Guard_sink
module Durable_guard = Tea_server.Durable_guard
module Replay_guard = Tea_server.Replay_guard

module Make_pack (A : Tea_core.App.APP) = struct
  module Store = Tea_persist_pack.Store_pack.Make (A)
  include Tea_server.Handlers (A) (Store)
  open Lwt.Syntax

  let checkpoint_path = "/admin/checkpoint"

  (** [POST /admin/checkpoint]: the same same-origin discipline as [accept_ws]
      — the only producer of the proof is {!Tea_safe.Origin_gate.check}, so a
      checkpoint accepted without it cannot be expressed. On a valid proof,
      squash [main], link the checkpoint onto the durable retention spine, and
      GC to the retention anchor. Every {!Tea_safe.Origin_gate.denial}, every
      {!Store.checkpoint_error}, and every {!Store.gc_error} is matched
      exhaustively. *)
  let handle_checkpoint ?(retention = Store.Retention.default) (repo : Store.t)
      (request : Dream.request) : Dream.response Lwt.t =
    Tea_safe.Origin_gate.check
      ~origin:(Dream.header request "Origin")
      ~host:(Dream.header request "Host")
    |> Result.fold
         ~error:(fun (d : Tea_safe.Origin_gate.denial) ->
           match d with
           | Origin_missing | Host_missing | Both_missing | Origin_mismatch ->
             Dream.respond ~status:`Forbidden "cross-origin checkpoint rejected")
         ~ok:(fun (_ : Tea_safe.Origin_gate.same_origin Tea_safe.Proof.t) ->
           let* main = Store.main_session repo in
           let* checkpointed = Store.checkpoint main ~label:"admin checkpoint" in
           Result.fold checkpointed
             ~error:(fun (e : Store.checkpoint_error) ->
               match e with
               | Empty_branch ->
                 Dream.respond ~status:`OK "checkpoint: empty branch, nothing to do"
               | Branch_moved ->
                 Dream.respond ~status:`Conflict "checkpoint: branch moved, retry")
             ~ok:(fun cp ->
               let* anchor = Store.retain repo ~retention cp in
               let* gced = Store.gc repo ~retain:anchor in
               Result.fold gced
                 ~error:(fun (ge : Store.gc_error) ->
                   match ge with
                   | Gc_disallowed ->
                     Dream.respond ~status:`Internal_Server_Error "gc disallowed"
                   | Gc_already_running -> Dream.respond ~status:`OK "gc already running"
                   | Gc_failed (_ : string) ->
                     Dream.respond ~status:`Internal_Server_Error "gc failed")
                 ~ok:(fun () ->
                   Dream.respond ~status:`OK
                     (Tea_core.Prim.Commit_ref.to_string (Store.checkpoint_ref cp)))))

  let checkpoint_route ?retention (repo : Store.t) : Dream.route =
    Dream.post checkpoint_path (handle_checkpoint ?retention repo)

  (** The full pack request pipeline: the mem tier's {!handler} with
      [POST /admin/checkpoint] folded into the RPC route list, so it inherits
      the session and security-header middleware like every other route. *)
  let handler_pack ?client_dir ?(rpc = []) ?coalesce ?retention ?guard
      (repo : Store.t) : Dream.handler =
    handler ?client_dir ~rpc:(checkpoint_route ?retention repo :: rpc) ?coalesce
      ?guard repo

  (** Blocking entry point for a durable pack server. A stop signal resolves
      Dream's [~stop] promise; once Dream returns, the repo is closed so the
      pack suffix is flushed to disk — mutation survival across a restart
      depends on this teardown close.

      Both SIGINT and SIGTERM are handled, and the second one matters as much as
      the first: SIGINT is what a terminal sends, but SIGTERM is what every
      supervisor sends — including the browser harness, which restarts this
      binary to observe that de-duplication survives a restart. A SIGTERM that
      bypassed this handler would kill the process before the teardown close,
      so the reopened store would be missing its most recent suffix and the
      scenario would be measuring a lost store rather than a lost delivery
      record. The wake is guarded by [is_sleeping], so a second signal (or both
      signals) resolves the promise once. *)
  let serve_pack ?(interface = "localhost") ?(port = 8080) ?client_dir ?rpc ?coalesce ?retention
      ?lower_root ~(root : Root.t) () : unit =
    let repo = Lwt_main.run (Store.create ?lower_root root) in
    (* The guard journal lives in a SIBLING directory of the pack root, never
       inside it: whether irmin-pack 3.11 tolerates a foreign file in its root
       across GC and migration is not a bet worth making. A failed open is one
       audible line and a null-sink guard — a server without durability beats
       no server, and the degradation direction is duplicate, never loss. *)
    let guard, journal =
      Lwt_main.run (Guard_file.open_ ~dir:(Root.to_string root ^ ".guard") ~cap:32768)
      |> Result.fold
           ~ok:(fun
               ((sink, floors, jf) :
                 Guard_sink.t * Durable_guard.Floors.t * Guard_file.t)
             ->
             ( Durable_guard.v ~sessions:Replay_guard.default_sessions
                 ~tabs:Replay_guard.default_tabs ~sink ~floors
             , Some jf ))
           ~error:(fun (e : Guard_file.open_err) ->
             let reason =
               match e with
               | Guard_file.Io reason -> reason
               | Guard_file.Bad_dir reason -> reason
             in
             Printf.eprintf
               "tea_server_pack: guard journal unavailable (%s); serving at-least-once\n%!"
               reason;
             ( Durable_guard.v ~sessions:Replay_guard.default_sessions
                 ~tabs:Replay_guard.default_tabs ~sink:Guard_sink.null
                 ~floors:Durable_guard.Floors.empty
             , None ))
    in
    let stop, wake = Lwt.wait () in
    List.iter
      (fun signal ->
        let (_ : Lwt_unix.signal_handler_id) =
          Lwt_unix.on_signal signal (fun (_ : int) ->
              if Lwt.is_sleeping stop then Lwt.wakeup_later wake ())
        in
        ())
      [ Sys.sigint; Sys.sigterm ];
    Lwt_main.run
      (Dream.serve ~interface ~port ~stop
         (Dream.logger
            (handler_pack ?client_dir ?rpc ?coalesce ?retention ~guard repo)));
    (* The REPO first, then the journal — the order is load-bearing. Both
       fsync on this path, and a crash between the two closes must land on
       the duplicate side: repo-first leaves commits durable and floors
       possibly behind (a replay reads Fresh, re-applies, visible duplicate);
       journal-first would leave floors durable over commits still in the
       pack's user-space buffer, and a replay would then read Duplicate
       against a commit that never reached disk — silent loss. "Floor
       durability never exceeds commit durability" is the invariant, and at
       teardown this ordering is what enforces it. *)
    Lwt_main.run
      (let open Lwt.Syntax in
       let* () = Store.close repo in
       Option.fold journal ~none:Lwt.return_unit ~some:Guard_file.close)
end
