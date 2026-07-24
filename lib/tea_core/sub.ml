type 'msg t =
  | None_
  | Batch of 'msg t list
  | Every of Prim.Interval.t * (int -> 'msg)

let none = None_
let batch xs = Batch xs
let every i f = Every (i, f)

let rec map f = function
  | None_ -> None_
  | Batch xs -> Batch (List.map (map f) xs)
  | Every (i, g) -> Every (i, fun tick -> f (g tick))
