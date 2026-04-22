use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/test_unitarity.sml";

fun assertTrue (b: bool, msg: string) = if b then () else raise Fail msg;

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val ok =
  TestUnitarity.test_minimal_spherical_unitary_points
    (g, Unitary.F4_spherical_unitary, false, SOME 3);
val () = AtlasFFI.atlas_group_free g;

val () = assertTrue (ok, "F4 spherical unitary sample did not pass");
val () = print "OK\n";

