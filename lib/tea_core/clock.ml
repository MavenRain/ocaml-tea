type t =
  { now : unit -> int64
  ; last : int64 Atomic.t
  }

type stamp = int64

(* Int64.min_int floor: the first [next] is the bare wall reading. *)
let create ~(now : unit -> int64) : t = { now; last = Atomic.make Int64.min_int }

let rec seed (t : t) (d : int64) : unit =
  let cur = Atomic.get t.last in
  if Int64.compare d cur <= 0 then ()
  else if Atomic.compare_and_set t.last cur d then ()
  else seed t d

let floor (t : t) : int64 = Atomic.get t.last

let rec next (t : t) : stamp =
  let cur = Atomic.get t.last in
  (* Saturate at max_int rather than wrap: strict monotonicity is this
     module's whole contract, and Int64 addition wraps silently. *)
  let bumped = if Int64.equal cur Int64.max_int then cur else Int64.add cur 1L in
  let candidate = Int64.max (t.now ()) bumped in
  if Atomic.compare_and_set t.last cur candidate then candidate else next t
