module Backoff = struct
  type t = int

  let base_ms = 500
  let cap_ms = 30_000
  let initial = base_ms
  let to_ms (b : t) : int = b

  (* [min] rather than a guard: the doubling is already bounded (a capped
     value doubles to 60s, nowhere near overflow), so saturation is the whole
     story and [bump cap = cap] falls out of it. *)
  let bump (b : t) : t = min (b * 2) cap_ms
end

type ('sock, 'timer) t =
  | Down
  | Opening of
      { sock : 'sock
      ; next : Backoff.t
      }
  | Up of 'sock
  | Waiting of
      { timer : 'timer
      ; next : Backoff.t
      }

let down = Down
let opening ~sock ~next = Opening { sock; next }
let waiting ~timer ~next = Waiting { timer; next }

let active (t : ('sock, 'timer) t) : bool =
  match t with
  | Down -> false
  | Opening { sock = (_ : 'sock); next = (_ : Backoff.t) } -> true
  | Up (_ : 'sock) -> true
  | Waiting { timer = (_ : 'timer); next = (_ : Backoff.t) } -> true

let sendable (t : ('sock, 'timer) t) : 'sock option =
  match t with
  | Up sock -> Some sock
  | Down -> None
  | Opening { sock = (_ : 'sock); next = (_ : Backoff.t) } -> None
  | Waiting { timer = (_ : 'timer); next = (_ : Backoff.t) } -> None

type close =
  | Stale
  | Reopen_after of
      { delay_ms : int
      ; next : Backoff.t
      }

(* The ladder is read off the state, not off a mutable field beside it: an
   [Opening] that never confirmed keeps escalating, while an [Up] that drops
   restarts at [initial] because reaching [Up] is the evidence that the server
   is reachable. *)
let reopen (next : Backoff.t) : close =
  Reopen_after { delay_ms = Backoff.to_ms next; next = Backoff.bump next }

let on_close ~(sock : 'sock) (t : ('sock, 'timer) t) : close =
  match t with
  | Opening { sock = w; next } -> if w == sock then reopen next else Stale
  | Up w -> if w == sock then reopen Backoff.initial else Stale
  | Down -> Stale
  | Waiting { timer = (_ : 'timer); next = (_ : Backoff.t) } -> Stale

let on_up ~(sock : 'sock) (t : ('sock, 'timer) t) : ('sock, 'timer) t =
  match t with
  | Opening { sock = w; next = (_ : Backoff.t) } -> if w == sock then Up w else t
  | Up (_ : 'sock) -> t
  | Down -> t
  | Waiting { timer = (_ : 'timer); next = (_ : Backoff.t) } -> t

type ('sock, 'timer) teardown =
  | Nothing
  | Close_socket of 'sock
  | Cancel_timer of 'timer

let stop (t : ('sock, 'timer) t) : ('sock, 'timer) teardown =
  match t with
  | Down -> Nothing
  | Opening { sock; next = (_ : Backoff.t) } -> Close_socket sock
  | Up sock -> Close_socket sock
  | Waiting { timer; next = (_ : Backoff.t) } -> Cancel_timer timer
