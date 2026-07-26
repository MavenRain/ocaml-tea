(** The irmin-pack backend shim: see store_pack.mli. *)

open Lwt.Syntax

(* irmin-pack 3.11 has no [Conf.Default]; these values follow conf.mli's own
   guidance: 32 entries per inode, no stable-hash compatibility baggage for a
   fresh store, and a varint length header so contents of any repr-derived
   codec parse unambiguously. *)
module Conf : Irmin_pack.Conf.S = struct
  let entries = 32
  let stable_hash = 0
  let contents_length_header = Some `Varint
  let inode_child_order = `Hash_bits
  let forbid_empty_dir_persistence = false
end

module Root = struct
  type t = Root of string

  let v (dir : string) : t = Root dir
  let to_string (Root dir : t) : string = dir
end

module Make (A : Tea_core.App.APP) = struct
  module Contents = Tea_persist.Store_core.Contents (A)
  module Maker_kv = Irmin_pack_unix.KV (Conf)
  module Pack = Maker_kv.Make (Contents)
  include Tea_persist.Store_core.Make (A) (Pack)

  (* ALWAYS minimal indexing: the default [always] strategy permanently
     poisons the root against delete-mode GC. And deliberately no [?fresh]:
     reopening must never silently truncate a durable store. *)
  let create ?(now = default_now) (root : Root.t) : t Lwt.t =
    let config =
      Irmin_pack.config
        ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal
        (Root.to_string root)
    in
    Lwt.bind (Pack.Repo.v config) (v ~now)

  type gc_error =
    | Gc_disallowed
    | Gc_already_running
    | Gc_failed of string

  (** [run] + [wait], never [start_exn]/[finalise_exn]: the result stays in
      [result] (house no-exceptions rule) and blocking until finalisation
      keeps tests deterministic. *)
  let gc (t : t) ~(retain : checkpoint) : (unit, gc_error) result Lwt.t =
    let r = repo t in
    if not (Pack.Gc.is_allowed r) then Lwt.return (Error Gc_disallowed)
    else
      let key = Pack.Commit.key (checkpoint_commit retain) in
      let* started = Pack.Gc.run r key in
      match started with
      | Error (`Msg m) -> Lwt.return (Error (Gc_failed m))
      | Ok false -> Lwt.return (Error Gc_already_running)
      | Ok true -> (
        let* finished = Pack.Gc.wait r in
        match finished with
        | Error (`Msg m) -> Lwt.return (Error (Gc_failed m))
        | Ok (_ : Irmin_pack_unix.Stats.Latest_gc.stats option) -> Lwt.return (Ok ()))
end
