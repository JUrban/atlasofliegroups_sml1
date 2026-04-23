use "atlas-scripts-sml/group_operations.sml";

(*
  Smoke test for `atlas-scripts-sml/group_operations.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rdA1 = RootDatum.newSimple (#"A", 1, false);
val rdA2 = RootDatum.newSimple (#"A", 2, false);
val rd = GroupOperations.* (rdA1, rdA2);

val _ = assert "product rank = 1+2" (RootDatum.rank rd = 3);
val _ = assert "product semisimple rank = 1+2" (RootDatum.semisimpleRank rd = 3);

val rad = GroupOperations.radical rd;
val _ = assert "radical rank=0 for semisimple root datum" (RootDatum.rank rad = 0);

val () = RootDatum.free rdA1;
val () = RootDatum.free rdA2;
val () = RootDatum.free rd;
val () = RootDatum.free rad;

val _ = print "ok\n";

