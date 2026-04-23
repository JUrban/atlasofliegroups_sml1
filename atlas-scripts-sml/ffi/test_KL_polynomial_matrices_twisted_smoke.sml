use "atlas-scripts-sml/KL_polynomial_matrices.sml";

(*
  Smoke test for the twisted (outer automorphism) KL helpers.
  We use delta = identity, so twisting should act trivially and the twisted
  KL P-matrix should match the untwisted one on a tiny example.
*)

fun assert msg b = if b then () else raise Fail ("assertion failed: " ^ msg)

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail ("group_new_simple failed: " ^ AtlasFFI.atlas_last_error ()) else ()
val p = AtlasFFI.atlas_param_trivial g
val () = if p = Foreign.Memory.null then raise Fail ("param_trivial failed: " ^ AtlasFFI.atlas_last_error ()) else ()

val delta = [[1]]

val P = KL_polynomial_matrices.KL_P_polynomials p
val Pt = KL_polynomial_matrices.KL_P_polynomials_twisted (p, delta)
val Ps = KL_polynomial_matrices.KL_P_signed_polynomials p
val Pst = KL_polynomial_matrices.KL_P_signed_polynomials_twisted (p, delta)

val () = assert "P=Pt (delta=I)" (Polynomial.equalMat (P, Pt))
val () = assert "Ps=Pst (delta=I)" (Polynomial.equalMat (Ps, Pst))

val () = AtlasFFI.atlas_param_free p
val () = AtlasFFI.atlas_group_free g
val () = print "OK: KL_polynomial_matrices_twisted_smoke\n"

