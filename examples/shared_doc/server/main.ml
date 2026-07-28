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

    This binary calls [Dream.run] directly on [Shared_doc_serve.handler] and is
    a MEM-tier server, so its session identity is deliberately per-process
    (roadmap step 12, D17): the model dies with the process, and a durable
    identity over a volatile store would send a reconnecting tab to a branch
    name that resolves to an empty branch. Identity durability must never
    exceed model durability. A durable shared_doc would need the pack tier
    first. *)

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
  let repo = Lwt_main.run (Server.Store.create ()) in
  Dream.run ~interface:"localhost" ~port
    (Dream.logger (Shared_doc_serve.handler ?client_dir repo))
