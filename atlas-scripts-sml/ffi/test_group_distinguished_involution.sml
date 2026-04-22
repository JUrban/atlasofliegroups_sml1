use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/twisted_root_datum.sml";

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val rd = AtlasFFI.atlas_group_rootdatum_new g;
val deltaText = AtlasFFI.atlas_group_distinguished_involution_text g;
val delta = IntMatrix.parseMatText deltaText;

val (n, m) = IntMatrix.matShape delta;
val () = if n = 4 andalso m = 4 then () else raise Fail "distinguished involution: bad shape";

val () =
  if TwistedRootDatum.is_distinguished (rd, delta) then ()
  else raise Fail "distinguished involution: not distinguished";

val () = RootDatum.free rd;
val () = AtlasFFI.atlas_group_free g;

val () = print "OK\n";
