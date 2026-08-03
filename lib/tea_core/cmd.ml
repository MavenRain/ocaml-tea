type http_failure =
  | Http_status of Prim.Status.t
  | Network_error
  | No_transport

module Http_delivery = struct
  type t =
    | Bare
    | Keyed
end

type 'msg t =
  | None_
  | Batch of 'msg t list
  | Emit of 'msg
  | After of Prim.Delay.t * 'msg
  | Navigate of Prim.Url.t
  | Http of
      { path : Prim.Rpc_path.t
      ; body : string
      ; delivery : Http_delivery.t
      ; expect : (string, http_failure) result -> 'msg
      }

let none = None_
let batch xs = Batch xs
let emit m = Emit m
let after d m = After (d, m)
let navigate u = Navigate u
let http ~path ~body ~expect = Http { path; body; delivery = Http_delivery.Bare; expect }

let http_keyed ~path ~body ~expect =
  Http { path; body; delivery = Http_delivery.Keyed; expect }

let rec map f = function
  | None_ -> None_
  | Batch xs -> Batch (List.map (map f) xs)
  | Emit m -> Emit (f m)
  | After (d, m) -> After (d, f m)
  | Navigate u -> Navigate u
  | Http { path; body; delivery; expect } ->
    Http { path; body; delivery; expect = (fun r -> f (expect r)) }
