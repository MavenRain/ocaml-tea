type 'msg t =
  | None_
  | Batch of 'msg t list
  | Emit of 'msg
  | After of Prim.Delay.t * 'msg
  | Navigate of Prim.Url.t

let none = None_
let batch xs = Batch xs
let emit m = Emit m
let after d m = After (d, m)
let navigate u = Navigate u

let rec map f = function
  | None_ -> None_
  | Batch xs -> Batch (List.map (map f) xs)
  | Emit m -> Emit (f m)
  | After (d, m) -> After (d, f m)
  | Navigate u -> Navigate u
