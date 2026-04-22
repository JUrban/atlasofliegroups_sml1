use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/one_dimensional.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/ffi/test_one_dimensional_nonexample_A1.sml

  Purpose
  - Sanity check: a nontrivial finite-dimensional representation of `A1_s`
    should not be classified as one-dimensional by `OneDimensional.is_one_dimensional`.

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_one_dimensional_nonexample_A1.sml`
*)

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val lam = Lattice.ratvecNormalize {den = 1, nums = [1]}
val p = Representations.finite_dimensional (g, lam)
val () = if OneDimensional.is_one_dimensional (g, p) then raise Fail "expected not one-dimensional" else ()

val () = AtlasFFI.atlas_param_free p
val () = AtlasFFI.atlas_group_free g

val () = TextIO.print "OK\n"

