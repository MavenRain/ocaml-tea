(** The D4 checkpoint retention ring on the durable [__checkpoints] spine
    (roadmap step 8).

    [retain ~retention:K] links each checkpoint onto a chained spine and
    returns the GC anchor. Two invariants, each anti-vacuous against a listed
    mutation:

    - "exactly K retained": with N > K checkpoints on the spine, the returned
      anchor sits exactly K-1 parents from the head (the K-th checkpoint
      counting the head as first). An off-by-one (walk K instead of K-1) would
      keep K+1 and move the anchor to index K.
    - "keys survive reopen": the spine ref, and every checkpoint reachable from
      it, survive a close/reopen. Dropping the spine write loses them. *)

module Store = Tea_persist_pack.Store_pack.Make (Counter_app.App)
module Prim = Tea_core.Prim

(* Hashes of the spine commits, head first, walking first parents. *)
let spine_hashes (t : Store.t) : string list Lwt.t =
  let open Lwt.Syntax in
  let hash (c : Store.S.commit) : string =
    Irmin.Type.to_string Store.S.Hash.t (Store.S.Commit.hash c)
  in
  let rec walk (c : Store.S.commit) (acc : string list) : string list Lwt.t =
    let acc = hash c :: acc in
    match Store.S.Commit.parents c with
    | [] -> Lwt.return (List.rev acc)
    | pkey :: _ -> (
      let* p = Store.S.Commit.of_key (Store.repo t) pkey in
      match p with
      | None -> Lwt.return (List.rev acc)
      | Some pc -> walk pc acc)
  in
  let* head = Store.checkpoints_head t in
  match head with
  | None -> Lwt.return []
  | Some cp -> walk (Store.checkpoint_commit cp) []

let index_of (x : string) (xs : string list) : int option =
  let rec go i = function
    | [] -> None
    | y :: rest -> if String.equal x y then Some i else go (i + 1) rest
  in
  go 0 xs

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Counter_app.App in
     let check name cond =
       if cond then Printf.printf "ok   - %s\n%!" name
       else (
         Printf.printf "FAIL - %s\n%!" name;
         exit 1)
     in
     let k = 3 in
     let n = 6 in
     let retention = Option.get (Prim.Retention.of_int k) in

     let parent = Filename.temp_dir "ocaml-tea-retention" "" in
     let root = Tea_persist_pack.Store_pack.Root.v (Filename.concat parent "store") in
     let* t = Store.create ~now:(fun () -> 1L) root in
     let* main = Store.main_session t in

     (* N rounds of (increment main; checkpoint; retain). The last anchor is
        the one whose position we measure. *)
     let* last_anchor =
       Lwt_list.fold_left_s
         (fun (_ : Store.checkpoint option) (_ : int) ->
           let* (_ : model) = Store.apply main Increment in
           let* cp_r = Store.checkpoint main ~label:"cp" in
           match cp_r with
           | Error (Empty_branch | Branch_moved) -> Lwt.return None
           | Ok cp ->
             let* anchor = Store.retain t ~retention cp in
             Lwt.return (Some anchor))
         None
         (List.init n (fun i -> i))
     in
     let anchor =
       match last_anchor with
       | Some a -> a
       | None -> failwith "no checkpoint was retained"
     in

     let* hashes = spine_hashes t in
     check "the spine chains one entry per checkpoint (length = N)" (List.length hashes = n);
     let anchor_hash =
       Irmin.Type.to_string Store.S.Hash.t (Store.S.Commit.hash (Store.checkpoint_commit anchor))
     in
     let pos = index_of anchor_hash hashes in
     check "the anchor is the K-th checkpoint from the head (index = K-1)"
       (pos = Some (k - 1));

     (* The spine head carries the model committed at round N (count = N). *)
     let* head0 = Store.checkpoints_head t in
     let* head_model0 =
       match head0 with
       | None -> Lwt.return (fst init)
       | Some cp -> Store.model_at t (Store.checkpoint_commit cp)
     in
     check "the spine head carries the latest checkpointed model (count = N)"
       (value head_model0 = n);

     (* Durability: the spine ref and its whole chain survive a close/reopen. *)
     let* () = Store.close t in
     let* t2 = Store.create ~now:(fun () -> 1L) root in
     let* hashes2 = spine_hashes t2 in
     check "the spine keys survive reopen (same chain length)" (List.length hashes2 = n);
     check "the spine chain is byte-identical after reopen" (hashes2 = hashes);
     let* head2 = Store.checkpoints_head t2 in
     let* head_model2 =
       match head2 with
       | None -> Lwt.return (fst init)
       | Some cp -> Store.model_at t2 (Store.checkpoint_commit cp)
     in
     check "the reopened spine head still carries count = N" (value head_model2 = n);
     let* () = Store.close t2 in

     let rec rm_rf (path : string) : unit =
       if Sys.is_directory path then (
         Array.iter (fun entry -> rm_rf (Filename.concat path entry)) (Sys.readdir path);
         Sys.rmdir path)
       else Sys.remove path
     in
     rm_rf parent;
     Printf.printf "\nThe retention ring keeps exactly K checkpoints and persists the spine (D4).\n%!";
     Lwt.return_unit)
