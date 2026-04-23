use "atlas-scripts-sml/KTypePol.sml";

(*
  File: atlas-scripts-sml/to_ht.sml

  Purpose
  - Minimal SML analogue of `atlas-scripts/to_ht.at`.
  - The full Atlas `.at` implementation includes many optimized variants for
    truncating hermitian/c-forms and KL sums to a given “height” bound.
  - For the SML port we start with:
      - truncating `KTypePol` by term height
      - building basic “form-to-height” helpers for parameters
      - implementing a purity-based `is_unitary_to_ht` predicate suitable for
        pruning (not as a final decision procedure)

  API
  - `ToHT.to_ht(pol, HT)`:
      returns a NEW `KTypePol` handle containing only terms with `height <= HT`.
      For `HT < 0`, returns a full copy (implemented via a large cutoff).
  - `ToHT.full_deform_to_ht(p,HT)`:
      computes `full_deform(p)` and truncates it.
  - `ToHT.hermitian_form_irreducible_to_ht(p,HT)` / `c_form_irreducible_to_ht(p,HT)`:
      compute the corresponding invariant form and truncate it.
  - `ToHT.is_unitary_to_ht(p,HT)`:
      returns `false` if the hermitian form already has a “mixed” coefficient
      at height <= HT; returns `true` otherwise (so it can yield false positives).

  Ownership
  - The returned `ktypepol` is a fresh handle and must be freed with
    `KTypePol.free` by the caller.
*)

structure ToHT = struct
  type ktypepol = AtlasFFI.ktypepol
  type param = AtlasFFI.param

  (* `Foreign.cInt` marshaling requires the argument fit in a C `int`, so we
     use an explicit large cutoff rather than `Int.maxInt` (which can be 63-bit
     on Poly/ML builds). *)
  val hugeCutoff : int = 2147483647

  fun to_ht (pol: ktypepol, ht: int) : ktypepol =
    if ht < 0 then
      KTypePol.toHT (pol, hugeCutoff)
    else
      KTypePol.toHT (pol, ht)

  (*
    Unitarity “to height” predicates

    The original `.at` implementation uses sophisticated truncated-form
    calculations (and cached/interrupted variants) to quickly DISPROVE unitarity
    at low “height” bounds.

    For the SML port we implement the core `.at` semantics:
    - for `HT < 0`, fall back to the exact Atlas predicate `is_unitary(p)`;
    - otherwise, compute `hermitian_form_irreducible(p)` (via FFI), truncate it
      and check coefficient-wise purity up to height `HT`.

    Implementation note
    - We avoid allocating an explicit truncated `KTypePol` by computing the
      first impure height once (via `KTypePol.impureHeight`) and comparing it
      to `HT`.

    This is a *pruning* predicate: it can return false positives but should not
    return false negatives.
  *)

  fun paramRank (p: param) : int =
    let
      val toks = String.tokens Char.isSpace (AtlasFFI.atlas_param_lambda_text p)
    in
      Int.max (0, length toks - 1)
    end

  fun full_deform_to_ht (p: param, ht: int) : ktypepol =
    let
      val pol = AtlasFFI.atlas_param_full_deform p
      val () = if pol = Foreign.Memory.null then raise Fail ("full_deform_to_ht: full_deform failed: " ^ AtlasFFI.atlas_last_error ()) else ()
    in
      if ht < 0 then
        pol
      else
        let
          val q = to_ht (pol, ht)
          val () = KTypePol.free pol
        in
          q
        end
    end

  fun hermitian_form_irreducible_to_ht (p: param, ht: int) : ktypepol =
    let
      val hf = AtlasFFI.atlas_param_hermitian_form_irreducible p
      val () =
        if hf = Foreign.Memory.null then
          raise Fail ("hermitian_form_irreducible_to_ht: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      if ht < 0 then
        hf
      else
        let
          val q = to_ht (hf, ht)
          val () = KTypePol.free hf
        in
          q
        end
    end

  fun c_form_irreducible_to_ht (p: param, ht: int) : ktypepol =
    let
      val cf = AtlasFFI.atlas_param_c_form_irreducible p
      val () =
        if cf = Foreign.Memory.null then
          raise Fail ("c_form_irreducible_to_ht: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      if ht < 0 then
        cf
      else
        let
          val q = to_ht (cf, ht)
          val () = KTypePol.free cf
        in
          q
        end
    end

  fun is_unitary_to_ht (p: param, ht: int) : bool =
    if ht < 0 then
      AtlasFFI.atlas_param_is_hermitian p = 1 andalso AtlasFFI.atlas_param_is_unitary p = 1
    else if AtlasFFI.atlas_param_is_hermitian p <> 1 then
      false
    else
      let
        val hf = AtlasFFI.atlas_param_hermitian_form_irreducible p
        val () =
          if hf = Foreign.Memory.null then
            raise Fail ("is_unitary_to_ht: hermitian_form_irreducible failed: " ^ AtlasFFI.atlas_last_error ())
          else
            ()
        val r = paramRank p
        val d = KTypePol.impureHeight (hf, r)
        val ok = (d = ~1) orelse (d > ht)
        val () = KTypePol.free hf
      in
        ok
      end

  fun is_unitary_to_hts (p: param, hts: int list) : bool =
    List.all (fn ht => is_unitary_to_ht (p, ht)) hts

  fun is_unitary_to_ht_prune_equal_rank (g: AtlasFFI.group, p: param, ht: int) : bool =
    if AtlasFFI.atlas_param_is_hermitian p <> 1 then
      false
    else
      let
        (* Equal-rank optimization: coefficient mixedness is preserved under the
           `c_form -> hermitian_form` conversion (multiplication by `s` only
           swaps integer/s parts), so for pruning we can inspect `c_form`. *)
        val hf = AtlasFFI.atlas_param_c_form_irreducible p
        val () =
          if hf = Foreign.Memory.null then
            raise Fail ("is_unitary_to_ht_prune_equal_rank: c_form_irreducible failed: " ^ AtlasFFI.atlas_last_error ())
          else
            ()
        val r = AtlasFFI.atlas_group_rank g
        (* Atlas `.at` tests coefficient-wise purity (`is_pure`), not the stronger
           “pure module” predicate. Using module purity here would be UNSAFE as a
           pruning test, because it could reject parameters whose truncated form
           has a mix of integer and `s*Z` coefficients but no mixed coefficients. *)
        val d = KTypePol.impureHeight (hf, r)
        val ok = (d = ~1) orelse (d > ht)
        val () = KTypePol.free hf
      in
        ok
      end

  (*
    Depth-style variant for pruning: returns the first impure height, or `~1`
    if the form is pure up to `ht` (and therefore not disproved at that bound).

    Atlas correspondence
    - Mirrors `is_unitary_to_ht_base_depth(p,HT)` from `to_ht.at` in the
      equal-rank branch, but in a simplified form:
        - returns `~2` for non-hermitian parameters
        - does not attempt to compute the full `is_unitary_depth` for `HT<0`
  *)
  fun is_unitary_to_ht_prune_equal_rank_depth (g: AtlasFFI.group, p: param, ht: int) : int =
    if AtlasFFI.atlas_param_is_hermitian p <> 1 then
      ~2
    else if ht < 0 then
      ~1
    else
      let
        (* Same equal-rank optimization as in `is_unitary_to_ht_prune_equal_rank`. *)
        val hf = AtlasFFI.atlas_param_c_form_irreducible p
        val () =
          if hf = Foreign.Memory.null then
            raise Fail ("is_unitary_to_ht_prune_equal_rank_depth: c_form_irreducible failed: " ^ AtlasFFI.atlas_last_error ())
          else
            ()
        val r = AtlasFFI.atlas_group_rank g
        val d0 = KTypePol.impureHeight (hf, r)
        val d = if d0 = ~1 orelse d0 > ht then ~1 else d0
        val () = KTypePol.free hf
      in
        d
      end
end
