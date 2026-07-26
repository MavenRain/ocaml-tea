module type LOCAL = sig
  type shared
  type msg
  type local

  val init : local
  val update : msg -> shared -> local -> (local * msg Cmd.t) option
  val view : shared -> local -> msg Html.t
end

module None_ (A : App.APP) = struct
  type shared = A.model
  type msg = A.msg
  type local = unit

  let init = ()

  (* Declines unconditionally: no message is inspected, so no arm can drift
     out of step with [A.msg]. This is what makes the parity in
     [Tea_client.Local_channel] structural rather than a claim to re-check
     whenever [A.msg] grows. *)
  let update (_ : msg) (_ : shared) (_ : local) : (local * msg Cmd.t) option = None
  let view (shared : shared) (_ : local) : msg Html.t = A.view shared
end
