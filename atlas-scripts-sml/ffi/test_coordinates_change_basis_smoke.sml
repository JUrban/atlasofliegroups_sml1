use "atlas-scripts-sml/coordinates_at.sml";

(*
  File: atlas-scripts-sml/ffi/test_coordinates_change_basis_smoke.sml

  Purpose
  - Smoke test for `CoordinatesAT.change_basis_*` on `A1`.
  - Using `my_roots = simple_roots(rd)` should yield identity.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = RootDatum.newSimple (#"A", 1, false);
val s = RootDatum.simpleRootsMat rd;
val my = RatMat.ofIntMat s;

val c = CoordinatesAT.change_basis_ratmat (rd, my);
val () = assert "change_basis(simple_roots)=I" (RatMat.equal (c, RatMat.id_mat 1));

val d = CoordinatesAT.inverse_change_basis_ratmat (rd, my);
val () = assert "inverse_change_basis(simple_roots)=I" (RatMat.equal (d, RatMat.id_mat 1));

val () = RootDatum.free rd;

