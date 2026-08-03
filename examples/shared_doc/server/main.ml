(** The native Shared-document server: the same [Shared_doc_app.App] served over
    Dream. SSR + form posts at [/] (buttons: like/unlike, add/remove tag), the
    live-view WebSocket at {!Tea_core.Wire.ws_path}, the typed RPC endpoints
    under {!Tea_rpc.prefix} (POST [/rpc/history_count], [/rpc/doc_stats], and
    the same-origin-gated [/rpc/append_tag]), and - when the compiled client
    bundle is on disk - the browser client at [/app]. Open [/app] in two tabs to
    watch one session's edits stream to the other (step 3 live view); the
    {i cross-session} three-way merge that this app exists to demonstrate is
    exercised end-to-end by [test/collab_test].

    This module is the entry point only: port and bundle discovery, then
    [Dream.run]. Everything routable lives in {!Shared_doc_serve}, which the
    test suite links directly.

    [TEA_ROOT] picks the durable backend, exactly as the counter binary does it
    (roadmap step 11, D16): unset, this is the MEM tier - [Dream.run] over
    [Shared_doc_serve.handler], a model that dies with the process, and a
    deliberately per-process session identity (step 12, D17), because a durable
    identity over a volatile store would send a reconnecting tab to a branch
    name that resolves to an empty branch, and identity durability must never
    exceed model durability. Set, it is the pack tier: the model, the session
    secret and the delivery floors all live under that directory.

    The durable arm arrived with roadmap step 15 (D20) and the keyed RPC channel
    is why. [/rpc/append_tag] is this app's one [Mutating] endpoint, and the
    guarantee the step states - a request whose response was lost is retried
    under the same key and applied exactly once - is a statement about what
    survives a restart. On the mem tier there is nothing to survive: the floors
    and the document die together, so a retry that crosses a boot re-applies,
    and the tier can offer at-least-once and no more. The browser harness's B10
    reads the durable statement end to end, which needs this arm to exist. *)

module Server = Shared_doc_serve.Server

let () =
  let port =
    Sys.getenv_opt "PORT"
    |> (fun o -> Option.bind o int_of_string_opt)
    |> Option.value ~default:8080
  in
  let built = "_build/default/examples/shared_doc/client" in
  let client_dir =
    Sys.getenv_opt "CLIENT_DIR"
    |> Option.fold
         ~none:
           (if Sys.file_exists built then Some built
            else (
              Printf.eprintf
                "shared_doc server: CLIENT_DIR unset and %s not found; /app disabled\n%!"
                built;
              None))
         ~some:Option.some
  in
  (* Closures on BOTH branches, applied once: [Option.fold]'s [~none:] is eager,
     so a bare [Dream.run ...] there would create the memory repo and start
     serving from it before the pack branch was ever consulted. *)
  Sys.getenv_opt "TEA_ROOT"
  |> Option.fold
       ~none:(fun () ->
         let repo = Lwt_main.run (Server.Store.create ()) in
         Dream.run ~interface:"localhost" ~port
           (Dream.logger (Shared_doc_serve.handler ?client_dir repo)))
       ~some:(fun (root : string) () ->
         (* No repo is created here: [serve_pack] opens the pack root itself
            (refusing an unusable one before it listens), and hands it back to
            the [?rpc_once] builder - which is why that builder takes a store.
            Two handles on one pack root is not a thing to arrange. *)
         Shared_doc_serve.Pack.serve ~port ?client_dir
           ~root:(Tea_server_pack.Root.v root) ())
  |> fun run -> run ()
