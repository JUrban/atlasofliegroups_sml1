use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/BigUnitaryHashStore.sml";

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val store = BigUnitaryHashStore.create ();
val j = BigUnitaryHashStore.rf_number store g;

val p = AtlasFFI.atlas_param_trivial g;
val k1 = BigUnitaryHashStore.long_match store (p, j);
val k2 = BigUnitaryHashStore.long_match store (p, j);

val () =
  print
    ("rf=" ^ Int.toString j ^ " k1=" ^ Int.toString k1 ^ " k2=" ^ Int.toString k2 ^ " size="
     ^ Int.toString (ParamHash.size (BigUnitaryHashStore.uhash store g))
     ^ "\n");

val () = AtlasFFI.atlas_param_free p;
val () = BigUnitaryHashStore.freeAll store;
val () = AtlasFFI.atlas_group_free g;

