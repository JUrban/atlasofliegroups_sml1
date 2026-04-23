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

    For the SML port we currently keep the default predicate *exact* by
    delegating to `AtlasFFI.atlas_param_is_unitary` (height ignored), because
    the correct truncated-form logic depends on equal-rank vs non-equal-rank
    branching and conversions that are not yet fully ported.

    A separate helper `is_unitary_to_ht_prune_equal_rank` is provided for
    experiments in equal-rank cases, based on truncating the hermitian form and
    checking module purity.
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
    let
      val _ = ht
    in
      AtlasFFI.atlas_param_is_hermitian p = 1 andalso AtlasFFI.atlas_param_is_unitary p = 1
    end

  fun is_unitary_to_hts (p: param, hts: int list) : bool =
    let
      val _ = hts
    in
      AtlasFFI.atlas_param_is_hermitian p = 1 andalso AtlasFFI.atlas_param_is_unitary p = 1
    end

  fun is_unitary_to_ht_prune_equal_rank (g: AtlasFFI.group, p: param, ht: int) : bool =
    if AtlasFFI.atlas_param_is_hermitian p <> 1 then
      false
    else
      let
        val hf = hermitian_form_irreducible_to_ht (p, ht)
        val r = AtlasFFI.atlas_group_rank g
        (* Atlas `.at` tests coefficient-wise purity (`is_pure`), not the stronger
           “pure module” predicate. Using module purity here would be UNSAFE as a
           pruning test, because it could reject parameters whose truncated form
           has a mix of integer and `s*Z` coefficients but no mixed coefficients. *)
        val ok = KTypePol.isPure (hf, r)
        val () = KTypePol.free hf
      in
        ok
      end
end
