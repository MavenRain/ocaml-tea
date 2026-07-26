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

(* THE CONTENTS FRAMING CONTRACT (this is what [contents_length_header] above
   actually promises irmin-pack).

   A pack entry is [hash][kind][the contents codec's own bytes]; nothing in
   [Pack_value.Of_contents.encode_bin] writes a length. The reader recovers the
   entry length by decoding a varint at the head of those bytes
   ([Pack_store.read_and_decode_entry_prefix]), so the contents codec MUST begin
   with one varint spanning everything after it — the property [Repr.string] has
   and a [Repr.record] does not: a record's leading varint frames only its FIRST
   field. A single-field model (the Counter) satisfies the contract by accident,
   which is why every existing pack test passed; any multi-field model (the D1
   CvRDT doc) made the reader stop short and the decoder walk off the end of the
   truncated buffer with [Invalid_argument "index out of bounds"].

   [framed] re-encodes the whole model as one [Repr.string], which restores the
   invariant for any app model whatsoever. It is deliberately here, beside the
   [Conf] that demands it, and not in the backend-generic [Store_core]: the mem
   backend has no entry framing and keeps the model's structural encoding.

   The [fallback] arm is the corrupt-bytes case, never taken for bytes we wrote
   (no exception in normal control flow). It cannot mask corruption either:
   irmin re-hashes what it decoded and a fallback model hashes to something else
   than the key that was asked for, so the read fails loudly as a corrupt store
   rather than silently returning [init]. *)
let framed (type a) (inner : a Repr.t) (fallback : a) : a Repr.t =
  let enc = Repr.unstage (Repr.to_bin_string inner) in
  let dec = Repr.unstage (Repr.of_bin_string inner) in
  Repr.map Repr.string
    (fun (s : string) -> Result.fold (dec s) ~ok:Fun.id ~error:(fun (_ : [ `Msg of string ]) -> fallback))
    enc

module Make (A : Tea_core.App.APP) = struct
  module Contents = struct
    include Tea_persist.Store_core.Contents (A)

    (* Same [type t = A.model] and same [merge]; only the wire framing of a
       stored entry differs from the mem backend's. *)
    let t : t Repr.t = framed A.model_t (fst A.init)
  end

  module Maker_kv = Irmin_pack_unix.KV (Conf)
  module Pack = Maker_kv.Make (Contents)
  include Tea_persist.Store_core.Make (A) (Pack)

  (* ALWAYS minimal indexing: the default [always] strategy permanently
     poisons the root against delete-mode GC. And deliberately no [?fresh]:
     reopening must never silently truncate a durable store.

     [?lower_root] (D5): when set, the config carries a lower-layer directory,
     which flips [Gc.behaviour] to [`Archive] — GC then moves discarded data to
     the lower layer instead of deleting it, so pre-checkpoint commits stay
     readable. [Irmin_pack.config] cannot carry a lower root, so the config is
     built through [Irmin_pack.Conf.init] (a superset with the same defaults);
     the raising [add_volume]/[split] volume API is never touched. *)
  let create ?(now = default_now) ?lower_root ?exploded (root : Root.t) : t Lwt.t =
    let config =
      Irmin_pack.Conf.init
        ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal
        ~lower_root
        (Root.to_string root)
    in
    Lwt.bind (Pack.Repo.v config) (v ~now ?exploded)

  (** Whether GC on this store archives discarded data to a lower layer
      ([`Archive], a [lower_root] was configured) or deletes it ([`Delete]).
      Exposed so archive retention is observable without running a GC. *)
  let gc_behaviour (t : t) : [ `Archive | `Delete ] = Pack.Gc.behaviour (repo t)

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
