use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/FPP_barycenters_fold.sml";

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val barys = FPP_barycenters_fold.barycenters_all g;
val n = length barys;
val () = if n = 9789 then () else raise Fail ("expected 9789 barycenters, got " ^ Int.toString n);
val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

