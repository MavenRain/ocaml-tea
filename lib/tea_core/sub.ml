type ('model, 'msg) t =
  | None_
  | Batch of ('model, 'msg) t list
  | Every of Prim.Interval.t * (int -> 'msg)
  | Store_watch of ('model -> 'msg)

let none = None_
let batch xs = Batch xs
let every i f = Every (i, f)
let store_watch f = Store_watch f

let rec map f = function
  | None_ -> None_
  | Batch xs -> Batch (List.map (map f) xs)
  | Every (i, g) -> Every (i, fun tick -> f (g tick))
  | Store_watch g -> Store_watch (fun model -> f (g model))
