use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Hermitian.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/unitary.sml";

fun assertTrue (b: bool, msg: string) = if b then () else raise Fail msg;

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);

fun checkOne nu =
  let
    val p = Representations.minimal_spherical_principal_series (g, nu)
    val u0 = AtlasFFI.atlas_param_is_unitary p = 1
    val u1 = Hermitian.is_unitary_via_hermitian_form (g, p)
    val () = AtlasFFI.atlas_param_free p
  in
    assertTrue (u0 = u1, "is_unitary disagrees with hermitian form purity")
  end;

val _ = List.app checkOne (List.take (Unitary.F4_spherical_unitary, 3));

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

