use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";

(*
  File: atlas-scripts-sml/extended_misc.sml

  Purpose
  - Partial SML translation of `atlas-scripts/extended_misc.at`.
  - This module focuses on the outer-twist utilities that are prerequisites for
    extended-group computations (e.g. `bigMatrices.at`):
      - `twist(delta,p)` for a distinguished automorphism/involution matrix `delta`
      - `is_fixed(delta,p)` for testing delta-fixed parameters
      - simple filters `fixed(delta, B)` / `fixed_block_of`

  Atlas correspondence
  - In the `.at` world, `twist(delta,p)` is implemented by explicitly twisting
    the KGB element and weight data.
  - In this SML port, we delegate to the C++ library operation
      `Rep_context::twisted(sr,delta)`
    via the shim export `atlas_param_twist_by_delta_text`.

  Representation
  - `delta` is represented as an integer matrix `IntMatrix.mat` (row-major).

  Ownership
  - `twist` returns a fresh parameter handle; callers must free it with
    `AtlasFFI.atlas_param_free`.
  - Functions that only test or filter do not take ownership of input params.
*)

structure Extended_misc = struct
  type param = AtlasFFI.param
  type mat = IntMatrix.mat

  fun twist (delta: mat, p: param) : param =
    let
      val q = AtlasFFI.atlas_param_twist_by_delta_text (p, IntMatrix.matToText delta)
    in
      if q = Foreign.Memory.null then
        raise Fail ("Extended_misc.twist: failed: " ^ AtlasFFI.atlas_last_error ())
      else
        q
    end

  fun is_fixed (delta: mat, p: param) : bool =
    let
      val q = twist (delta, p)
      val eq = AtlasFFI.atlas_param_equal (p, q) = 1
      val () = AtlasFFI.atlas_param_free q
    in
      eq
    end

  fun fixed (delta: mat, ps: param list) : param list =
    List.filter (fn p => is_fixed (delta, p)) ps

  (* Convenience: delta-fixed elements from the full block of `p`. *)
  fun fixed_block_of (delta: mat, p: param) : param list =
    let
      val h = AtlasFFI.atlas_param_block_survivors p
      val () =
        if h = Foreign.Memory.null then
          raise Fail ("Extended_misc.fixed_block_of: block_survivors failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val n = AtlasFFI.atlas_paramlist_size h
      fun get i = AtlasFFI.atlas_paramlist_get_param_clone (h, i)
      val terms = List.tabulate (n, fn i => get i)
      val () = AtlasFFI.atlas_paramlist_free h
      val keep = fixed (delta, terms)
      val () =
        List.app
          (fn q =>
             if List.exists (fn k => AtlasFFI.atlas_param_equal (q, k) = 1) keep then ()
             else AtlasFFI.atlas_param_free q)
          terms
    in
      keep
    end
end

