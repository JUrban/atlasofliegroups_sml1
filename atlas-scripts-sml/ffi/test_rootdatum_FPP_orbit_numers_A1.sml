use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Lattice.sml";

val rd = RootDatum.newSimple (#"A", 1, false);
val gamma : Lattice.ratvec = {den = 1, nums = [0]};
val m = RootDatum.FPP_orbit_numers (rd, gamma);
val (r, c) = IntMatrix.matShape m;
val () = if c = 1 andalso r > 0 then () else raise Fail "FPP_orbit_numers: unexpected shape";
val () = RootDatum.free rd;
val () = print "OK\n";

