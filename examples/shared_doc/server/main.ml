(** The native Shared-document server: the same [Shared_doc_app.App] served over
    Dream. SSR + form posts at [/] (buttons: like/unlike, add/remove tag), the
    live-view WebSocket at {!Tea_core.Wire.ws_path}, and - when the compiled
    client bundle is on disk - the browser client at [/app]. Open [/app] in two
    tabs to watch one session's edits stream to the other (step 3 live view);
    the {i cross-session} three-way merge that this app exists to demonstrate is
    exercised end-to-end by [test/collab_test]. *)

module Server = Tea_server.Make (Shared_doc_app.App)

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
  Server.serve ~port ?client_dir ()
