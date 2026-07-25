(** The native Counter server: the shared [Counter_app.App] served over Dream.
    SSR + form posts at [/] (roadmap step 1), the live-view WebSocket at
    {!Tea_core.Wire.ws_path}, and — when the compiled client bundle is on
    disk — the browser client at [/app] (roadmap step 3). Open [/app] in two
    tabs, or [/] and [/app] side by side: every commit shows up everywhere. *)

module Server = Tea_server.Make (Counter_app.App)

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
  Server.serve ~port ?client_dir ()
