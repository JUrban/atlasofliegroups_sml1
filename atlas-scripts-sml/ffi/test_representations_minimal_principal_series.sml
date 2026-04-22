use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/representations.sml";

fun assertTrue (b: bool, msg: string) = if b then () else raise Fail msg;

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);

val p0 = Representations.minimal_principal_series_default g;
val h0 = AtlasFFI.atlas_param_height p0;
val () = assertTrue (h0 >= 0, "minimal_principal_series_default: bad height (C++ error?)");
val () = AtlasFFI.atlas_param_free p0;

val nu = Representations.rho g;
val p1 = Representations.minimal_spherical_principal_series (g, nu);
val h1 = AtlasFFI.atlas_param_height p1;
val () = assertTrue (h1 >= 0, "minimal_spherical_principal_series: bad height (C++ error?)");
val () = AtlasFFI.atlas_param_free p1;

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

