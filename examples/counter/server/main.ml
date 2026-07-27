(** The native Counter server: the shared [Counter_app.App] served over Dream.
    SSR + form posts at [/] (roadmap step 1), the live-view WebSocket at
    {!Tea_core.Wire.ws_path}, and — when the compiled client bundle is on
    disk — the browser client at [/app] (roadmap step 3). Open [/app] in two
    tabs, or [/] and [/app] side by side: every commit shows up everywhere.

    [TEA_ROOT] picks the durable backend (roadmap step 11, D16): with it set,
    the model and the delivery records live in an irmin-pack store under that
    directory and survive a restart; unset, the whole store is in memory and
    dies with the process. Both are real configurations, so the choice is an
    environment variable rather than two binaries — and it is the pack one the
    browser harness restarts, because a restart of the memory one would be
    measuring a store that was never asked to remember anything. *)

module Server = Tea_server.Make (Counter_app.App)
module Pack_server = Tea_server_pack.Make_pack (Counter_app.App)

let () =
  let port =
    Sys.getenv_opt "PORT"
    |> (fun o -> Option.bind o int_of_string_opt)
    |> Option.value ~default:8080
  in
  (* Where `dune build` drops the jsoo client. [CLIENT_DIR] overrides; if
     neither resolves, the server still runs SSR + WS, just without /app. *)
  let built = "_build/default/examples/counter/client" in
  let client_dir =
    Sys.getenv_opt "CLIENT_DIR"
    |> Option.fold
         ~none:
           (if Sys.file_exists built then Some built
            else (
              (* Audible, like every other degraded path in this framework:
                 losing /app silently would make the 404 undiagnosable. *)
              Printf.eprintf
                "counter server: CLIENT_DIR unset and %s not found; /app disabled\n%!"
                built;
              None))
         ~some:Option.some
  in
  (* Closures on BOTH branches, applied once: [Option.fold]'s [~none:] is
     eager, so a bare [Server.serve ...] there would start the memory server
     before the pack branch was ever consulted. *)
  Sys.getenv_opt "TEA_ROOT"
  |> Option.fold
       ~none:(fun () -> Server.serve ~port ?client_dir ())
       ~some:(fun (root : string) () ->
         Pack_server.serve_pack ~port ?client_dir ~root:(Tea_server_pack.Root.v root) ())
  |> fun run -> run ()
