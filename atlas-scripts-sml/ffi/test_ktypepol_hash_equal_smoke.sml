use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/KTypePolHash.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val t = KType.ofParam p;

val pol1 = KType.K_type_formula (t, 10);
val pol2 = KTypePol.clone pol1;
val pol3 = KTypePol.scaleSplit (pol1, 2, 0);

val _ = assert "equal(pol1,pol2)" (AtlasFFI.atlas_ktypepol_equal (pol1, pol2) = 1);
val _ = assert "not equal(pol1,pol3)" (AtlasFFI.atlas_ktypepol_equal (pol1, pol3) = 0);

val h1 = AtlasFFI.atlas_ktypepol_hash_code (pol1, 1024);
val h2 = AtlasFFI.atlas_ktypepol_hash_code (pol2, 1024);
val _ = assert "hash(pol1)=hash(pol2)" (h1 = h2 andalso h1 >= 0);

val ht = KTypePolHash.create 64;
val i1 = KTypePolHash.match ht pol1;
val i2 = KTypePolHash.match ht pol2;
val i3 = KTypePolHash.match ht pol3;
val _ = assert "match(pol1)=match(pol2)" (i1 = i2);
val _ = assert "match(pol3) different" (i3 <> i1);

val _ = KTypePolHash.freeAll ht;

val _ = KTypePol.free pol1;
val _ = KTypePol.free pol2;
val _ = KTypePol.free pol3;
val _ = KType.free t;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "OK\n";

