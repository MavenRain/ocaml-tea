(** The Shared document in the browser: the same [Shared_doc_app.App] the native
    server serves, compiled to JavaScript. Live edits arrive as [Sync_doc]
    frames over the store-watch WebSocket. *)

module Client = Tea_client_run.Start (Shared_doc_app.App)

let () = Client.boot ()
