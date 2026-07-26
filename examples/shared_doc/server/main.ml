(** The native Shared-document server: the same [Shared_doc_app.App] served over
    Dream. SSR + form posts at [/] (buttons: like/unlike, add/remove tag), the
    live-view WebSocket at {!Tea_core.Wire.ws_path}, the typed RPC endpoints
    under {!Tea_rpc.prefix} (POST [/rpc/history_count], [/rpc/doc_stats]), and -
    when the compiled client bundle is on disk - the browser client at [/app].
    Open [/app] in two tabs to watch one session's edits stream to the other
    (step 3 live view); the {i cross-session} three-way merge that this app
    exists to demonstrate is exercised end-to-end by [test/collab_test]. *)

module Server = Tea_server.Make (Shared_doc_app.App)
module Rpc = Tea_server.Rpc (Shared_doc_rpc)

(* The rank-2 handler dispatches every endpoint at its own type; distinct
   req/resp indices make a codec transposition a unification error. It is
   request-free by contract (it cannot see the Dream session), so
   [History_count] counts commits on the canonical branch. The handler and the
   request pipeline are built over the SAME repo — [Server.handler] is the
   documented test/embedding seam — so the count is the [main] branch this
   server actually serves, with no reliance on backend instance sharing. *)
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
  let handle : type a b. (a, b) Shared_doc_rpc.t -> a -> b Lwt.t =
   fun ep req ->
    match ep with
    | Shared_doc_rpc.History_count ->
      Lwt.bind (Server.Store.main_session repo) Server.Store.history
      |> Lwt.map List.length
    | Shared_doc_rpc.Doc_stats -> Lwt.return (Shared_doc_rpc.stats_of req)
  in
  Dream.run ~interface:"localhost" ~port
    (Dream.logger
       (Server.handler ?client_dir ~rpc:(Rpc.routes { Rpc.handle = handle }) repo))
