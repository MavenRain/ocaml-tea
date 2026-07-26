(** The streaming request-body cap (roadmap step 8, D11).

    Two layers, because a status code alone cannot tell the two failure modes
    apart:

    - the {!Tea_server.Body} loop is driven directly over a {i counting} chunk
      source, which is what makes the "never fully buffered" claim observable:
      the check is on bytes {e pulled}, not on the answer. A post-read cap -
      the code this replaces - would pass every status check below while
      pulling the attacker's whole body;
    - the real Dream route, with the real 64 KiB cap, over the shared_doc
      server: cap-1 and cap admitted (and their bodies reassembled byte for
      byte through the chunked reader), cap+1 refused 413. *)

module Serve = Shared_doc_serve
module Server = Serve.Server
module R = Tea_rpc.Make (Shared_doc_rpc)
module Body = Tea_server.Body

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

(* A chunk source over [s] handing out at most [size] bytes per pull, plus the
   running total of bytes it has actually surrendered. The counter is the whole
   point: it distinguishes "refused" from "refused without reading it all". *)
let counting_source ~(size : int) (s : string) :
    (unit -> string option Lwt.t) * (unit -> int) =
  let pulled = ref 0 in
  let read () =
    let remaining = String.length s - !pulled in
    if remaining <= 0 then Lwt.return None
    else (
      let n = Stdlib.min size remaining in
      let chunk = String.sub s !pulled n in
      pulled := !pulled + n;
      Lwt.return (Some chunk))
  in
  (read, fun () -> !pulled)

let capped ~max ~size s =
  let read, pulled = counting_source ~size s in
  let outcome = Lwt_main.run (Body.read_capped ~max ~read) in
  (outcome, pulled ())

let admitted = function
  | Body.Within_cap body -> Some body
  | Body.Body_too_large -> None

(* --- the cap boundary, at the seam ---------------------------------------- *)

let () =
  let cap = 32 in
  let at n = String.make n 'x' in
  let under, under_pulled = capped ~max:cap ~size:8 (at (cap - 1)) in
  check "a body of cap-1 bytes is admitted"
    (admitted under = Some (at (cap - 1)));
  check "an admitted body is pulled in full" (under_pulled = cap - 1);
  let exact, exact_pulled = capped ~max:cap ~size:8 (at cap) in
  check "a body of exactly cap bytes is admitted (the boundary is inclusive)"
    (admitted exact = Some (at cap));
  check "the exactly-cap body is pulled in full" (exact_pulled = cap);
  let over, (_ : int) = capped ~max:cap ~size:8 (at (cap + 1)) in
  check "a body of cap+1 bytes is refused" (Option.is_none (admitted over));
  check "an empty body is admitted as the empty string"
    (admitted (fst (capped ~max:cap ~size:8 "")) = Some "");
  (* Chunk boundaries must not be able to change the verdict: 1-byte pulls
     cross the cap on a different iteration than 8-byte pulls do. *)
  check "the verdict is independent of chunk size at the boundary"
    (List.for_all
       (fun size ->
         admitted (fst (capped ~max:cap ~size (at cap))) = Some (at cap)
         && Option.is_none (admitted (fst (capped ~max:cap ~size (at (cap + 1))))))
       [ 1; 3; 8; 31; 32; 33; 4096 ])

(* --- the claim the cap exists for: an oversized body is never buffered ---- *)

let () =
  let cap = 32 in
  let huge = String.make 4096 'x' in
  let over, pulled = capped ~max:cap ~size:8 huge in
  check "a hugely oversized body is refused" (Option.is_none (admitted over));
  check "the refusal happens pre-buffer: bytes pulled << body length"
    (pulled < String.length huge);
  (* Exactly one chunk past the cap: with 8-byte pulls, 40 is the first total
     that would exceed 32, and the loop refuses that chunk instead of adding it
     (so the buffer itself never passes the cap). Pinned as a number, because a
     rewrite that drains the stream before deciding - the pre-D11 behaviour -
     keeps every status code correct and only moves THIS value. *)
  check "the pull stops at the first chunk that crosses the cap (peak = cap + 1 chunk)"
    (pulled = 40);
  check "a 1-byte-chunk source stops one byte past the cap"
    (snd (capped ~max:cap ~size:1 huge) = cap + 1)

(* --- the same cap through the real Dream route ---------------------------- *)

let json = "application/json"

let rpc_post driver ~path payload =
  driver
    (Dream.request ~method_:`POST ~target:path
       ~headers:[ ("Content-Type", json) ]
       payload)

let body response = Lwt_main.run (Dream.body response)
let status response = Dream.status_to_int (Dream.status response)

(* A [Doc_stats] request whose ENCODED length is exactly [n] bytes: pad the doc
   body with alternating "x " so the reply's [word_count] counts the padding.
   That is what makes the within-cap checks non-vacuous - a truncating or
   chunk-dropping reader still decodes, but returns the wrong count. *)
let stats_payload_of_size n =
  let encode body = R.encode_req Shared_doc_rpc.Doc_stats { Shared_doc_rpc.title = "t"; body } in
  let overhead = String.length (encode "") in
  let pad = n - overhead in
  let doc = String.init pad (fun i -> if i mod 2 = 1 then ' ' else 'x') in
  (encode doc, (pad + 1) / 2)

let word_count_of response =
  R.decode_resp Shared_doc_rpc.Doc_stats (body response)
  |> Result.fold
       ~error:(fun (_ : Tea_core.Codec.err) -> -1)
       ~ok:(fun (s : Shared_doc_rpc.stats_resp) -> s.word_count)

let () =
  let cap = Serve.Rpc.max_body_bytes in
  let repo = Lwt_main.run (Server.Store.create ()) in
  let driver = Dream.test (Serve.handler repo) in
  let post_stats n =
    let payload, words = stats_payload_of_size n in
    check (Printf.sprintf "a %d-byte rpc body encodes to exactly %d bytes" n n)
      (String.length payload = n);
    (rpc_post driver ~path:"/rpc/doc_stats" payload, words)
  in
  let r_under, words_under = post_stats (cap - 1) in
  check "a body of cap-1 bytes is served 200" (status r_under = 200);
  check "the cap-1 body arrives byte-intact through the chunked reader"
    (word_count_of r_under = words_under);
  let r_exact, words_exact = post_stats cap in
  check "a body of exactly cap bytes is served 200 (boundary is inclusive)"
    (status r_exact = 200);
  check "the exactly-cap body arrives byte-intact through the chunked reader"
    (word_count_of r_exact = words_exact);
  let payload_over, (_ : int) = stats_payload_of_size (cap + 1) in
  let r_over = rpc_post driver ~path:"/rpc/doc_stats" payload_over in
  check "a body of cap+1 bytes is refused 413" (status r_over = 413);
  check "the 413 body names the cap, not the payload"
    (String.equal (body r_over) "rpc body too large");
  check "the 413 refusal is text/plain; charset=utf-8 (unsniffable)"
    (Dream.header r_over "Content-Type" = Some "text/plain; charset=utf-8");
  (* The cap is a transport gate, so it precedes decoding: an oversized body
     that is ALSO undecodable must still answer 413, never 400. *)
  check "an oversized undecodable body is 413, not 400 (cap precedes decode)"
    (status (rpc_post driver ~path:"/rpc/doc_stats" (String.make (cap + 1) 'x')) = 413);
  Printf.printf
    "\nThe rpc body cap is enforced while the body streams (roadmap step 8, D11).\n%!"
