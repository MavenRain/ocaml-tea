(** The Shared document in the browser: the same [Shared_doc_app.App] the native
    server serves, compiled to JavaScript. Live edits arrive as [Sync_doc]
    frames over the store-watch WebSocket, rebased onto whatever this tab is
    holding (roadmap step 8, D9).

    Mounted with [Start_local] and the app's own [Local] companion (D10): the
    stats readout is per-client, so neither its request nor its reply crosses
    the socket. *)

module Client = Tea_client_run.Start_local (Shared_doc_app.App) (Shared_doc_app.Local)

let () = Client.boot ()
