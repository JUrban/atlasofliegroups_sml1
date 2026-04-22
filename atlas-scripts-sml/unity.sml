use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/unity.sml

  Purpose
  - Initial SML analogue of selected utilities from `atlas-scripts/unity.at`.
  - The original `.at` file provides a large toolkit for *unitarity testing with
    height-based pruning*, including sophisticated “to height” variants.

  What is implemented here
  - Height lists derived from `full_deform(p)`:
      - `term_heights(pol, rank)`: sorted unique heights in a `KTypePol`
      - `off_hts(pol, rank)`: same list, but with the minimum height removed
      - `full_deform_term_heights(p)`: `term_heights(full_deform(p))` (frees `KTypePol`)
      - `full_deform_off_hts(p)`: `off_hts(full_deform(p))` (frees `KTypePol`)
      - `next_heights(p, depth)`: first `depth` values of `full_deform_off_hts(p)`
  - Baseline unitarity predicates (exact):
      - `is_unitary_test(p)`
      - `is_unitary_test_hts(p, hts)` (currently ignores `hts`)

  Notes and limitations
  - In `.at`, `short_hts`/`next_heights` are tuned to avoid doing full KL
    computations in hard cases. This SML baseline computes heights directly
    from `full_deform(p)`, which is correct but may be slower.
  - In `.at`, `is_unitary_test` implements height-stepped pruning; here we
    currently delegate to `AtlasFFI.atlas_param_is_unitary`.

  Ownership
  - Any `KTypePol` handle returned by the FFI is freed in these helpers.
*)

structure Unity = struct
  type param = AtlasFFI.param

  fun paramRank (p: param) : int =
    let
      val toks = String.tokens Char.isSpace (AtlasFFI.atlas_param_lambda_text p)
    in
      Int.max (0, length toks - 1)
    end

  fun term_heights (pol: KTypePol.ktypepol, rank: int) : int list =
    let
      val ts = KTypePol.terms (pol, rank)
      val hs = List.map (fn t => #height t) ts
    in
      Basic.sort_u (op <=) hs
    end

  (* Heights excluding the minimum (if any). *)
  fun off_hts (pol: KTypePol.ktypepol, rank: int) : int list =
    (case term_heights (pol, rank) of
       [] => []
     | _ :: rest => rest)

  fun full_deform_term_heights (p: param) : int list =
    let
      val pol = AtlasFFI.atlas_param_full_deform p
      val () = if pol = Foreign.Memory.null then raise Fail ("Unity.full_deform_term_heights: full_deform failed: " ^ AtlasFFI.atlas_last_error ()) else ()
      val rank = paramRank p
      val hs = term_heights (pol, rank)
      val () = KTypePol.free pol
    in
      hs
    end

  fun full_deform_off_hts (p: param) : int list =
    let
      val pol = AtlasFFI.atlas_param_full_deform p
      val () = if pol = Foreign.Memory.null then raise Fail ("Unity.full_deform_off_hts: full_deform failed: " ^ AtlasFFI.atlas_last_error ()) else ()
      val rank = paramRank p
      val hs = off_hts (pol, rank)
      val () = KTypePol.free pol
    in
      hs
    end

  (* First `depth` off-heights; returns fewer if there aren't enough. *)
  fun next_heights (p: param, depth: int) : int list =
    let
      val hs = full_deform_off_hts p
      val d = Int.max (0, depth)
    in
      List.take (hs, Int.min (d, length hs))
    end

  (* Exact unitarity predicate used as a baseline replacement for `.at`'s
     `is_unitary_test` logic. *)
  fun is_unitary_test (p: param) : bool =
    AtlasFFI.atlas_param_is_hermitian p = 1 andalso AtlasFFI.atlas_param_is_unitary p = 1

  fun is_unitary_test_hts (p: param, hts: int list) : bool =
    let
      val _ = hts
    in
      is_unitary_test p
    end
end
