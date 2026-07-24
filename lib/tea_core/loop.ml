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

  let rec drive ~fx ~fuel model cmd : (A.model, err) result Io.t =
    match Prim.Fuel.burn fuel with
    | None -> Io.return (Error Fuel_exhausted)
    | Some fuel' -> interpret ~fx ~fuel:fuel' model cmd

  and interpret ~fx ~fuel model (cmd : A.msg Cmd.t) : (A.model, err) result Io.t =
    match cmd with
    | Cmd.None_ -> Io.return (Ok model)
    | Cmd.Emit msg ->
      let model', cmd' = A.update msg model in
      drive ~fx ~fuel model' cmd'
    | Cmd.After (delay, msg) ->
      let* () = fx.sleep delay in
      let model', cmd' = A.update msg model in
      drive ~fx ~fuel model' cmd'
    | Cmd.Navigate url ->
      let* () = fx.navigate url in
      Io.return (Ok model)
    | Cmd.Batch cmds -> fold ~fx ~fuel model cmds

  and fold ~fx ~fuel model = function
    | [] -> Io.return (Ok model)
    | c :: rest -> (
      let* r = interpret ~fx ~fuel model c in
      match r with
      | Ok model' -> fold ~fx ~fuel model' rest
      | Error Fuel_exhausted -> Io.return (Error Fuel_exhausted))

  (** Apply one message and settle its command tail. *)
  let step ~fx ~fuel msg model : (A.model, err) result Io.t =
    let model', cmd = A.update msg model in
    drive ~fx ~fuel model' cmd
end
