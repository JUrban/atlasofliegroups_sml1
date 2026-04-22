use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/cofolded.sml";

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val (rdB, m, j0) = Cofolded.cofolded g;

val () = if RootDatum.rank rdB = 4 then () else raise Fail "cofolded: unexpected rank";

val (mr, mc) = IntMatrix.matShape m;
val () = if mr = 4 then () else raise Fail "cofolded: bad M rows";
val () = if mc > 0 then () else raise Fail "cofolded: zero columns";

val () = if j0 = ~1 then () else raise Fail ("cofolded: expected j0=-1, got " ^ Int.toString j0);

val () = RootDatum.free rdB;
val () = AtlasFFI.atlas_group_free g;

val () = print "OK\n";

