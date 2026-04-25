use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/tensor_product_sl2.sml";

(*
  File: atlas-scripts-sml/tensor_product_A1.sml

  Purpose
  - Incremental SML translation of `atlas-scripts/tensor_product_A1.at`.
  - The `.at` script provides helpers for tensor products of K-representations
    when `G` has semisimple rank 1, distinguishing the cases:
      - `G_der = SL(2,R)`  (connected maximal compact)
      - `G_der = PGL(2,R)` (disconnected maximal compact)

  What is implemented
  - `sl2_or_pgl2(g)`:
      return `0` for the SL(2,R)-type case and `1` for the PGL(2,R)-type case,
      based on `kgb_size` heuristics.
  - `tensor_sl2(mu,tau)`:
      a conservative approximation based on adding `lambda_minus_rho` vectors
      (implemented by `Tensor_product_sl2.tensor_sl2`), returned as a singleton
      `KTypePol`.
  - `tensor_gl2(mu,tau)`:
      currently implemented as the same approximation as `tensor_sl2` (see
      notes below).
  - `tensor_A1(mu,tau)`:
      choose `tensor_sl2` vs `tensor_gl2` using `sl2_or_pgl2`.

  Differences vs the `.at` implementation (important)
  - The `.at` `tensor_gl2` path uses `highest_weights`, `K_type_formula`, and
    `branch_std` to correctly handle the disconnected-K (GL2/O2) case. That
    infrastructure is not yet ported in SML, so `tensor_gl2` is currently an
    approximation that may be incorrect for PGL(2,R)-type groups.

  Ownership
  - The returned `AtlasFFI.ktypepol` handle is owned by the caller and must be
    freed with `KTypePol.free`.
*)

structure Tensor_product_A1 = struct
  type group = AtlasFFI.group
  type ktype = KType.ktype
  type ktypepol = AtlasFFI.ktypepol

  fun fail msg = raise Fail ("Tensor_product_A1: " ^ msg)

  fun groupOfKType (t: ktype) : group =
    let
      val p = KType.parameter t
      val g = AtlasFFI.atlas_param_group_handle p
      val () = AtlasFFI.atlas_param_free p
    in
      if g = Foreign.Memory.null then fail "groupOfKType: null group" else g
    end

  (*
    `.at`: sl2_or_pgl2(RealForm G)

    The `.at` script’s assertions appear to be inconsistent with the documented
    expected KGB sizes (2 or 3). Here we implement a simple, explicit check:

    - semisimple rank must be 1
    - kgb_size = 3  => return 0 (SL2-like)
    - kgb_size = 2  => return 1 (PGL2-like)
    - otherwise     => raise `Fail`
  *)
  fun sl2_or_pgl2 (g: group) : int =
    let
      val ssr = AtlasFFI.atlas_group_semisimple_rank g
      val () = if ssr = 1 then () else fail "sl2_or_pgl2: semisimple_rank != 1"
      val n = AtlasFFI.atlas_group_kgb_size g
    in
      if n = 3 then 0 else if n = 2 then 1 else fail ("sl2_or_pgl2: unexpected kgb_size " ^ Int.toString n)
    end

  (* Helper: return `tensor_sl2` as a singleton polynomial. *)
  fun tensor_sl2 (mu: ktype, tau: ktype) : ktypepol =
    let
      val t = Tensor_product_sl2.tensor_sl2 (mu, tau)
      val pol =
        (KTypePol.singleton (t, 1, 0)
         handle e => (KType.free t; raise e))
      val () = KType.free t
    in
      pol
    end

  (* Placeholder for the `.at` disconnected-K implementation.
     Currently uses the same approximation as `tensor_sl2`. *)
  fun tensor_gl2 (mu: ktype, tau: ktype) : ktypepol =
    tensor_sl2 (mu, tau)

  fun tensor_A1 (mu: ktype, tau: ktype) : ktypepol =
    let
      val gMu = groupOfKType mu
      val gTau = groupOfKType tau
      val () = if gMu = gTau then () else fail "tensor_A1: K-types from different groups"
    in
      if sl2_or_pgl2 gMu = 0 then tensor_sl2 (mu, tau) else tensor_gl2 (mu, tau)
    end
end

