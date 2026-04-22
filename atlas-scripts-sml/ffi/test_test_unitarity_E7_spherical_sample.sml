use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/test_unitarity.sml";

fun assertTrue (b: bool, msg: string) = if b then () else raise Fail msg;

val g = AtlasFFI.atlas_group_new_simple (#"E", 7, #"s", 0);
val ok =
  TestUnitarity.test_minimal_spherical_unitary_points
    (g, Unitary.E7_spherical_unitary, false, SOME 1);
val () = AtlasFFI.atlas_group_free g;

val () = assertTrue (ok, "E7 spherical unitary sample did not pass");
val () = print "OK\n";

