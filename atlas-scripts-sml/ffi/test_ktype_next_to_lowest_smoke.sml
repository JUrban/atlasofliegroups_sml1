use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/representations.sml";

(*
  Smoke test for:
  - `atlas_ktype_height`
  - `atlas_ktype_next_to_lowest`
  - SML wrapper `KType.nextToLowest`
*)

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0);
val p = Representations.trivial g;
val mu = KType.ofParam p;
val h0 = KType.height mu;

val nu = KType.nextToLowest mu;
val h1 = KType.height nu;
val _ = if h1 <= h0 then raise Fail ("expected next height > base: " ^ Int.toString h0 ^ " -> " ^ Int.toString h1) else ();

val _ = KType.free nu;
val _ = KType.free mu;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "OK\n";

