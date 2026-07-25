(** The client↔server live-view wire surface (roadmap step 3, DESIGN §7).

    Both tiers link this module, so the endpoint path cannot drift between
    them — the same no-drift discipline as the shared [APP] itself. The frame
    payloads need no definition here: they are the app's own Repr-JSON codecs
    ({!Codec}) — ['msg] frames travel up the socket, full ['model] frames
    travel down after every commit. *)

val ws_path : string
(** The WebSocket endpoint every ocaml-tea server mounts and every live-view
    client dials. *)
