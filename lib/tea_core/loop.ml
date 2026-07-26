(** The server-side TEA engine.

    [Loop] is parameterised over an {!IO} monad so the pure core never mentions
    Lwt: the server instantiates it with Lwt, tests with the identity monad.
    [step] applies [update] once, then interprets the returned [Cmd] — folding
    any [Emit]/[After] messages back through [update] — until the command
    settles ([Cmd.none]) or [fuel] is exhausted. The match over [Cmd] is
    exhaustive; adding a constructor is a compile error here. *)

module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val all : 'a t list -> 'a list t
end

module Loop (Io : IO) (A : App.APP) = struct
  type fx =
    { sleep : Prim.Delay.t -> unit Io.t
    ; navigate : Prim.Url.t -> unit Io.t
    }

  type err = Fuel_exhausted

  let ( let* ) = Io.bind

  let rec drive ~ctx ~fx ~fuel model cmd : (A.model, err) result Io.t =
    match Prim.Fuel.burn fuel with
    | None -> Io.return (Error Fuel_exhausted)
    | Some fuel' -> interpret ~ctx ~fx ~fuel:fuel' model cmd

  and interpret ~ctx ~fx ~fuel model (cmd : A.msg Cmd.t) : (A.model, err) result Io.t =
    match cmd with
    | Cmd.None_ -> Io.return (Ok model)
    | Cmd.Emit msg ->
      let model', cmd' = A.update ctx msg model in
      drive ~ctx ~fx ~fuel model' cmd'
    | Cmd.After (delay, msg) ->
      let* () = fx.sleep delay in
      let model', cmd' = A.update ctx msg model in
      drive ~ctx ~fx ~fuel model' cmd'
    | Cmd.Navigate url ->
      let* () = fx.navigate url in
      Io.return (Ok model)
    | Cmd.Http { path = (_ : Prim.Rpc_path.t); body = (_ : string); expect } ->
      (* This tier has no HTTP client: fail closed INTO the app, not past it.
         The continuation is fed back through [update] like [Emit], so the
         reply msg is fuel-bounded and the app decides what a transportless
         call means. Swapping this arm for an [fx.http] field is the
         compile-forced upgrade path if server-side dispatch is ever wanted. *)
      let model', cmd' = A.update ctx (expect (Error Cmd.No_transport)) model in
      drive ~ctx ~fx ~fuel model' cmd'
    | Cmd.Batch cmds -> fold ~ctx ~fx ~fuel model cmds

  and fold ~ctx ~fx ~fuel model = function
    | [] -> Io.return (Ok model)
    | c :: rest -> (
      let* r = interpret ~ctx ~fx ~fuel model c in
      match r with
      | Ok model' -> fold ~ctx ~fx ~fuel model' rest
      | Error Fuel_exhausted -> Io.return (Error Fuel_exhausted))

  (** Apply one message and settle its command tail. [ctx] is the CRDT context
      the message and every folded-back [Emit]/[After] is applied under. *)
  let step ~ctx ~fx ~fuel msg model : (A.model, err) result Io.t =
    let model', cmd = A.update ctx msg model in
    drive ~ctx ~fx ~fuel model' cmd
end
