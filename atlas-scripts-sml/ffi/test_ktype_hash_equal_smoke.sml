use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/KTypeHash.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val t1 = KType.ofParam p;
val t2 = AtlasFFI.atlas_ktype_clone t1;
val t3 = KType.nextToLowest t1;

val _ = assert "t2 clone" (t2 <> Foreign.Memory.null);
val _ = assert "equal(t1,t2)" (AtlasFFI.atlas_ktype_equal (t1, t2) = 1);
val _ = assert "not equal(t1,t3)" (AtlasFFI.atlas_ktype_equal (t1, t3) = 0);

val h1 = AtlasFFI.atlas_ktype_hash_code (t1, 1024);
val h2 = AtlasFFI.atlas_ktype_hash_code (t2, 1024);
val _ = assert "hash(t1)=hash(t2)" (h1 = h2 andalso h1 >= 0);

val ht = KTypeHash.create 64;
val i1 = KTypeHash.match ht t1;
val i2 = KTypeHash.match ht t2;
val i3 = KTypeHash.match ht t3;
val _ = assert "match(t1)=match(t2)" (i1 = i2);
val _ = assert "match(t3) different" (i3 <> i1);

val _ = KTypeHash.freeAll ht;

val _ = KType.free t3;
val _ = AtlasFFI.atlas_ktype_free t2;
val _ = KType.free t1;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "OK\n";

