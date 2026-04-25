use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/AtlasParam.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/polynomial.sml";
use "atlas-scripts-sml/KL_polynomial_matrices.sml";

(*
  File: atlas-scripts-sml/dual.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/dual.at`.
  - The original `.at` script is “Vogan-duality adjacent”: it contains
    convenience routines for dual blocks and for inspecting KL P/Q polynomial
    matrices.

  Status (important)
  - The *block duality* pieces depend on the `.at`-level Vogan duality layer
    (`Vogan-dual.at`) which is not yet ported (and not fully exposed via the
    current SML FFI). Those entry points are therefore stubs.
  - The KL-matrix helpers that do *not* require Vogan duality are implemented:
      - `KL_Q_polynomial` (indexing into a precomputed Q-matrix)
      - `test_duality` (sanity check that P_signed * Q = I on a block)

  Ownership
  - `test_duality` uses `Representations.block_of` which returns freshly-cloned
    `Param` handles; those clones are freed internally before returning.
*)

structure Dual = struct
  type param = AtlasFFI.param
  type i_poly = Polynomial.i_poly
  type i_poly_mat = Polynomial.i_poly_mat

  fun expect (where': string, b: bool, msg: string) : unit =
    if b then () else raise Fail ("Dual." ^ where' ^ ": " ^ msg)

  fun find_in_block (B: param list, p: param) : int =
    let
      fun loop ([], _) = ~1
        | loop (q :: qs, i) =
            if AtlasParam.eq (q, p) then i else loop (qs, i + 1)
    in
      loop (B, 0)
    end

  (*
    dual_block

    Atlas `.at`:
      dual_block(B,dual_inner_class) = (B_vee, perm)
      dual_block(B) = dual_block(B, dual_inner_class(B[0]))

    In SML, the required Vogan-duality primitives are not yet available, so
    these remain as placeholders.
  *)
  fun dual_block (B: param list) : param list * int list =
    let
      val _ = B
    in
    raise Fail "Dual.dual_block: not yet ported (requires Vogan duality)"
    end

  fun dual_block_with_inner (B: param list, dual_inner_class: string) : param list * int list =
    let
      val _ = (B, dual_inner_class)
    in
    raise Fail "Dual.dual_block_with_inner: not yet ported (requires Vogan duality)"
    end

  (*
    KL_Q_polynomial(B,Q,irr,std)

    Atlas `.at`:
      - `irr`/`std` are parameters in the block `B`.
      - `Q` is a precomputed Q-matrix for that block.
      - Returns the entry `Q[index_irr][index_std]`.
  *)
  fun KL_Q_polynomial (B: param list, Q: i_poly_mat, irr: param, std: param) : i_poly =
    let
      val index_irr = find_in_block (B, irr)
      val index_std = find_in_block (B, std)
      val () = expect ("KL_Q_polynomial", index_irr >= 0 andalso index_std >= 0, "irr and/or std not found in block")
    in
      List.nth (List.nth (Q, index_irr), index_std)
    end

  (*
    test_duality(p)

    Atlas `.at` intention:
      - Let `B = block_of(p)`.
      - Compute signed P-matrix and Q-matrix for the same basis.
      - Check `P_signed * Q = I`.

    Implementation notes
    - We compute block survivors using the C++ primitive exposed via
      `Representations.block_of`.
    - We use the SML KL helpers:
        `KL_polynomial_matrices.KL_P_signed_polynomials_B`
        `KL_polynomial_matrices.KL_Q_polynomials_B`
    - We always free the temporary block parameter handles before returning.
  *)
  fun test_duality (p: param) : i_poly_mat * i_poly_mat * bool =
    let
      val B = Representations.block_of p
      val P_signed =
        (KL_polynomial_matrices.KL_P_signed_polynomials_B B
         handle e => (List.app AtlasFFI.atlas_param_free B; raise e))
      val Q =
        (KL_polynomial_matrices.KL_Q_polynomials_B B
         handle e => (List.app AtlasFFI.atlas_param_free B; raise e))
      val () = List.app AtlasFFI.atlas_param_free B

      val R = Polynomial.matMul (P_signed, Q)
      val n = length P_signed
      val success = Polynomial.equalMat (R, Polynomial.identity_poly_matrix n)
    in
      (P_signed, Q, success)
    end
end
