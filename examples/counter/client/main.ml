(** The Counter in the browser (roadmap step 2): the same [Counter_app.App]
    the native server serves, compiled to JavaScript — thesis T3's client
    half. *)

module Client = Tea_client_run.Start (Counter_app.App)

let () = Client.boot ()
