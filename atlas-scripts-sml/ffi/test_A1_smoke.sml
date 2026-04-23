use "atlas-scripts-sml/A1.sml";
use "atlas-scripts-sml/lietypes.sml";

(*
  Smoke test for `atlas-scripts-sml/A1.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = A1.A1_root_datum (2, 1); (* A1 x A1 x T1 *)
val lt = RootDatum.lieType rd;
val _ = assert "semisimple part is A1.A1" (LieTypes.to_canonical lt = [(#"A", 1), (#"A", 1)]);

val center = A1.central_subgroup_as_ratvec [[1, 0, 1]];
val _ = assert "center vector is denom=2" (#den (List.hd center) = 2);

val () = RootDatum.free rd;
val _ = print "ok\n";
