use "atlas-scripts-sml/bigMatrices.sml";
use "atlas-scripts-sml/representations.sml";

(*
  Smoke test for `BigMatrices`.
  We take a tiny block and delta = identity; in that case the “big” P matrix
  construction should succeed and produce a unitriangular matrix.
*)

fun assert msg b = if b then () else raise Fail ("assertion failed: " ^ msg)

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail ("group_new_simple failed: " ^ AtlasFFI.atlas_last_error ()) else ()
val p = AtlasFFI.atlas_param_trivial g
val () = if p = Foreign.Memory.null then raise Fail ("param_trivial failed: " ^ AtlasFFI.atlas_last_error ()) else ()

val B = Representations.block_of p
val delta = [[1]]
val bigP = BigMatrices.big_KL_P_polynomials (B, delta)

val (n, m) = Polynomial.shape bigP
val () = assert "square" (n = m andalso n > 0)
val () = assert "diag=1" (List.all (fn i => (List.nth (List.nth (bigP, i), i)) = Polynomial.poly_1) (List.tabulate (n, fn i => i)))

val () = List.app AtlasFFI.atlas_param_free B
val () = AtlasFFI.atlas_param_free p
val () = AtlasFFI.atlas_group_free g
val () = print "OK: bigMatrices_smoke\n"

