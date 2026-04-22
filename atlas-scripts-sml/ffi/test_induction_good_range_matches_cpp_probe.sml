use "atlas-scripts-sml/G2_unitary_dual.sml";
use "atlas-scripts-sml/induction.sml";

fun assertEq msg (a: string, b: string) =
  if a = b then () else raise Fail (msg ^ "\nexpected: " ^ a ^ "\nactual: " ^ b);

val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0);
val ps = G2_unitary_dual.p_s g (G2_unitary_dual.ratOfInt 3, {num = ~1, den = 2});
val p = hd ps;

val txtSml = Induction.good_range_induced_from_first_text (p, g);
val txtCpp = AtlasFFI.atlas_param_good_range_induced_from_first_text p;

val () = assertEq "SML good-range result differs from C++ probe" (txtCpp, txtSml);

val () = List.app AtlasFFI.atlas_param_free ps;
val () = AtlasFFI.atlas_group_free g;

val () = print ("OK: " ^ txtSml ^ "\n");

