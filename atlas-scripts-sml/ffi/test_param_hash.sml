use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamHash.sml";

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val h = ParamHash.create 64;

val j1 = ParamHash.match h p;
val j2 = ParamHash.match h p;
val () = print ("j1=" ^ Int.toString j1 ^ " j2=" ^ Int.toString j2 ^ " size=" ^ Int.toString (ParamHash.size h) ^ "\n");

val q = ParamHash.index h j1;
val () = print ("equal(index,p)=" ^ Int.toString (AtlasFFI.atlas_param_equal (q, p)) ^ "\n");

val () = ParamHash.freeAll h;
val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;

