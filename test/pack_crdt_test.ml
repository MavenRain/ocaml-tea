(** Durability of a multi-field CvRDT model on irmin-pack (roadmap step 8, D1/D6).

    Every other pack test drives the single-field Counter, whose encoding happens
    to begin with a varint spanning the whole value — exactly the framing
    irmin-pack demands of a contents codec. A multi-field model's own encoding
    begins with its FIRST field's header instead, so the reader took a field
    length for the entry length, handed the decoder a truncated buffer, and
    [load] died with [Invalid_argument "index out of bounds"] on the first
    reopen. {!Tea_persist_pack.Store_pack.framed} restores the invariant for any
    app model; this test pins both the contract and the round trip it buys. *)

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    incr failures)

(* irmin-pack's own reader: decode the varint at the head of the contents bytes
   and take it as the length of everything after it (LEB128, as [Repr] writes
   it). [None] when those bytes are not a length at all. *)
let self_framed (type a) (t : a Repr.t) (v : a) : bool =
  let buf = Buffer.create 256 in
  Repr.(unstage (encode_bin t)) v (Buffer.add_string buf);
  let s = Buffer.contents buf in
  let rec varint (pos : int) (shift : int) (acc : int) : (int * int) option =
    if pos >= String.length s then None
    else
      let byte = Char.code s.[pos] in
      let acc = acc + ((byte land 127) lsl shift) in
      if byte < 128 then Some (acc, pos + 1) else varint (pos + 1) (shift + 7) acc
  in
  Option.fold (varint 0 0 0) ~none:false ~some:(fun ((len : int), (after : int)) ->
      after + len = String.length s)

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Shared_doc_app.App in
     let module Prim = Tea_core.Prim in
     let module Store = Tea_persist_pack.Store_pack.Make (Shared_doc_app.App) in
     let sid s = Option.get (Prim.Session_id.of_string s) in
     let parent = Filename.temp_dir "ocaml-tea-pack-crdt" "" in
     let root_at (name : string) =
       Tea_persist_pack.Store_pack.Root.v (Filename.concat parent name)
     in

     (* ---- (a) The framing contract, on its own ---------------------------- *)
     let sample =
       let ctx =
         Tea_core.Crdt.Ctx.v
           ~clock:(Tea_core.Clock.create ~now:(fun () -> 7L))
           ~replica:(Tea_core.Crdt.Replica.v (sid "framing"))
       in
       List.fold_left (fun m msg -> fst (update ctx msg m)) bottom
         [ Set_title "Treatise"; Set_body (String.make 400 'x'); Like; Add_tag "urgent" ]
     in
     check "the model's own encoding does NOT self-frame (this is the bug)"
       (not (self_framed model_t sample));
     check "the framed contents codec self-frames (short model)"
       (self_framed (Tea_persist_pack.Store_pack.framed model_t bottom) bottom);
     check "the framed contents codec self-frames (model past a 1-byte varint)"
       (self_framed (Tea_persist_pack.Store_pack.framed model_t bottom) sample);
     let framed_rt =
       let t = Tea_persist_pack.Store_pack.framed model_t bottom in
       let buf = Buffer.create 256 in
       Repr.(unstage (encode_bin t)) sample (Buffer.add_string buf);
       Repr.(unstage (decode_bin t)) (Buffer.contents buf) (ref 0)
     in
     check "the framed codec round-trips the model unchanged"
       (Repr.(unstage (equal model_t)) framed_rt sample);

     (* ---- (b) Whole-model durability across close/reopen ------------------- *)
     let root = root_at "whole" in
     let* t = Store.create ~now:(fun () -> 42L) root in
     let* main = Store.main_session t in
     let* (_ : model) = Store.apply main (Set_title "Treatise") in
     let* (_ : model) = Store.apply main (Set_body (String.make 400 'x')) in
     let* (_ : model) = Store.apply main Like in
     let* m4 = Store.apply main (Add_tag "urgent") in
     check "four edits land (likes = 1, one tag)" (likes_of m4 = 1 && tags_of m4 = [ "urgent" ]);
     let* () = Store.close t in

     let* t2 = Store.create ~now:(fun () -> 43L) root in
     let* main2 = Store.main_session t2 in
     let* m_re =
       Lwt.catch
         (fun () -> Lwt.map Option.some (Store.load main2))
         (fun e ->
           Printf.printf "note - load raised: %s\n%!" (Printexc.to_string e);
           Lwt.return None)
     in
     check "close/reopen: the multi-field CvRDT model reloads at all"
       (Option.is_some m_re);
     check "close/reopen: every field survives"
       (Option.fold m_re ~none:false ~some:(fun m ->
            title_of m = "Treatise"
            && body_of m = String.make 400 'x'
            && likes_of m = 1
            && tags_of m = [ "urgent" ]));
     let* hist = Store.history main2 in
     check "close/reopen: history is intact (one commit per update)" (List.length hist = 4);
     (* An edit after the reopen writes on top of the reloaded state. *)
     let* m_more = Store.apply main2 Like in
     check "close/reopen: a further edit builds on the reloaded state (likes = 2)"
       (likes_of m_more = 2 && title_of m_more = "Treatise");
     let* () = Store.close t2 in

     (* ---- (c) Durability under the D6 exploded witness --------------------- *)
     let field_key (n : string) = Tea_safe.Safe_key.(root (Step.v n)) in
     let witness () =
       Store.exploder ~bottom ~join:doc_join
         ~fields:
           (List.map (fun ((n : string), project) -> (field_key n, project)) explode_fields)
     in
     let xroot = root_at "exploded" in
     let* x = Store.create ~now:(fun () -> 42L) ~exploded:(witness ()) xroot in
     let* xs = Store.session x (sid "explodepk") in
     let* (_ : model) = Store.apply xs (Set_title "Scattered") in
     let* (_ : model) = Store.apply xs Like in
     let* (_ : model) = Store.apply xs (Add_tag "review") in
     let* () = Store.close x in

     let* x2 = Store.create ~now:(fun () -> 43L) ~exploded:(witness ()) xroot in
     let* xs2 = Store.session x2 (sid "explodepk") in
     let* mx =
       Lwt.catch
         (fun () -> Lwt.map Option.some (Store.load xs2))
         (fun e ->
           Printf.printf "note - exploded load raised: %s\n%!" (Printexc.to_string e);
           Lwt.return None)
     in
     check "exploded (D6) close/reopen: the leaves reassemble by join"
       (Option.fold mx ~none:false ~some:(fun m ->
            title_of m = "Scattered" && likes_of m = 1 && tags_of m = [ "review" ]));
     let* () = Store.close x2 in

     (* Cleanup: both stores are confined to the scratch dir. *)
     let rec rm_rf (path : string) : unit =
       if Sys.is_directory path then (
         Array.iter (fun entry -> rm_rf (Filename.concat path entry)) (Sys.readdir path);
         Sys.rmdir path)
       else Sys.remove path
     in
     rm_rf parent;
     if !failures = 0 then (
       print_string
         "\nA multi-field CvRDT model is durable on the pack backend, whole and exploded.\n";
       Lwt.return_unit)
     else (
       Printf.printf "\n%d check(s) FAILED\n" !failures;
       exit 1))
