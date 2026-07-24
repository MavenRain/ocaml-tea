(** The native Counter server: the shared [Counter_app.App] served over Dream
    (roadmap step 1). SSR + form posts; no client JS, no WebSocket. *)

module Server = Tea_server.Make (Counter_app.App)

let () =
  let port =
    Sys.getenv_opt "PORT"
    |> (fun o -> Option.bind o int_of_string_opt)
    |> Option.value ~default:8080
  in
  Server.serve ~port ()
