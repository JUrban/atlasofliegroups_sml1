use "atlas-scripts-sml/KL_polynomial_matrices.sml";

(*
  Smoke test for `KL_polynomial_matrices`.
  - Computes KL P and signed P for a tiny block (A1, split, trivial parameter).
  - Checks unitriangular shape properties and that Q (defined as inverse of
    signed P in this port) indeed inverts it.
*)

fun assert msg b = if b then () else raise Fail ("assertion failed: " ^ msg)

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail ("group_new_simple failed: " ^ AtlasFFI.atlas_last_error ()) else ()

val p = AtlasFFI.atlas_param_trivial g
val () = if p = Foreign.Memory.null then raise Fail ("param_trivial failed: " ^ AtlasFFI.atlas_last_error ()) else ()

val P = KL_polynomial_matrices.KL_P_polynomials p
val Ps = KL_polynomial_matrices.KL_P_signed_polynomials p
val Q = KL_polynomial_matrices.KL_Q_polynomials p

val (n, m) = Polynomial.shape P
val () = assert "square" (n = m andalso n > 0)

fun polyEq (a, b) = (Polynomial.strip a) = (Polynomial.strip b)

fun checkUnitriangular mat =
  let
    val (r, c) = Polynomial.shape mat
    val () = assert "square2" (r = c)
    fun entry i j = List.nth (List.nth (mat, i), j)
    fun loop i j =
      if i = r then ()
      else if j = c then loop (i + 1) 0
      else
        (if i = j then assert "diag=1" (polyEq (entry i j, Polynomial.poly_1))
         else if i > j then assert "below=0" (polyEq (entry i j, Polynomial.poly_0))
         else ();
         loop i (j + 1))
  in
    loop 0 0
  end

val () = checkUnitriangular P
val () = checkUnitriangular Ps

val I = Polynomial.identity_poly_matrix n
val R = Polynomial.matMul (Ps, Q)
val () = assert "Ps*Q=I" (Polynomial.equalMat (R, I))

val () = AtlasFFI.atlas_param_free p
val () = AtlasFFI.atlas_group_free g
val () = print "OK: KL_polynomial_matrices_smoke\n"

