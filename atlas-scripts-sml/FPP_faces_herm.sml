use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/BigUnitaryCache.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/sort.sml";

(*
  File: atlas-scripts-sml/FPP_faces_herm.sml

  Purpose
  - Incremental SML port of `atlas-scripts/FPP_faces_herm.at`.
  - Provides:
      - cached hermitian/unitary predicates used by the FPP scripts
      - “next height” helpers (`next_to_lowest`, `next_heights`) used for
        choosing truncation bounds in `to_ht`/bottom-layer style pruning

  Notes
  - The `.at` file is large; we add helpers as translated scripts require them.
*)
structure FPP_faces_herm = struct
  type param = AtlasFFI.param
  type ktype = AtlasFFI.ktype

  type unitary_cache = BigUnitaryCache.t

  (* Allocate a fresh unitary cache. *)
  fun make_unitary_cache () : unitary_cache =
    BigUnitaryCache.create 4096

  (* Cached unitary predicate with a fast hermitian short-circuit. *)
  fun is_unitary_hash_big_SIMPLE (cache: unitary_cache) (p: param) : bool =
    if AtlasFFI.atlas_param_is_hermitian p <> 1 then
      false
    else
      BigUnitaryCache.check_unitary cache p

  (* ---------------------------------------------------------------------- *)
  (* “Next height” helpers (partial port).                                   *)
  (* ---------------------------------------------------------------------- *)

  (* Next-to-lowest K-type above `mu` (caller must free). *)
  fun next_to_lowest (mu: ktype) : ktype =
    KType.nextToLowest mu

  (* Heights of the next-to-lowest sequence above a lowest K-type.

     Corresponds to `.at`:
       `next_heights(KType mu, int D)`.

     Ownership
     - Does not free the input `mu`.
  *)
  fun next_heights_ktype (mu: ktype, d: int) : int list =
    if d <= 0 then
      []
    else
      let
        fun loop (0, cur, ownsCur, acc) =
          (if ownsCur then KType.free cur else (); List.rev acc)
          | loop (k, cur, ownsCur, acc) =
              let
                val nxt = next_to_lowest cur
                val h = KType.height nxt
                val () = if ownsCur then KType.free cur else ()
              in
                loop (k - 1, nxt, true, h :: acc)
              end
      in
        loop (d, mu, false, [])
      end

  (* Heights used by the FPP scripts for a parameter, as a merged unique list.

     Corresponds to `.at`:
       `next_heights(Param p, int D)`.

     Notes
     - The `.at` version concatenates the per-LKT sequences, sorts/uniques, and
       truncates to length `D`.
  *)
  fun next_heights (p: param, d: int) : int list =
    if d <= 0 then
      []
    else
      let
        val lkts = Representations.LKTs p

        fun collect ((mu, _mult), acc) =
          let
            val hs = next_heights_ktype (mu, d)
            val () = KType.free mu
          in
            hs @ acc
          end

        val hs = List.foldl collect [] lkts
        val hs = Sort.sort_u (op <=) hs
      in
        List.take (hs, Int.min (d, length hs))
      end
end
