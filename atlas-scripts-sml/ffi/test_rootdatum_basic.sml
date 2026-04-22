use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = RootDatum.newSimple (#"F", 4, false);
val _ = assert "rank(F4)=4" (RootDatum.rank rd = 4);

val pos = RootDatum.posRootsCols rd;
val _ = assert "F4 has 24 posroots" (length pos = 24);
val _ = assert "posroots have dim 4" (List.all (fn v => length v = 4) pos);

val rho = RootDatum.rhoText rd;
val _ = assert "rho text nonempty" (String.size rho > 0);

val cartan =
  [ [2, ~1, 0, 0]
  , [~1, 2, ~2, 0]
  , [0, ~1, 2, ~1]
  , [0, 0, ~1, 2]
  ];
val id4 = IntMatrix.identity 4;
val rd2 = RootDatum.newFromSimpleMats (cartan, id4, true);
val _ = assert "rank(from mats)=4" (RootDatum.rank rd2 = 4);

val () = RootDatum.free rd;
val () = RootDatum.free rd2;

val _ = print "ok\n";

