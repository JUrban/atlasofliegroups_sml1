use "atlas-scripts-sml/lietypes.sml";

(*
  Smoke test for `atlas-scripts-sml/lietypes.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val lt0 : LieType.t = [(#"C", 2), (#"A", 1), (#"T", 1)];
val can0 = LieTypes.to_canonical lt0;
val _ = assert "to_canonical maps C2->B2 and sorts" (can0 = [(#"A", 1), (#"B", 2), (#"T", 1)]);

val _ = assert "is_isomorphic compares canonical forms"
               (LieTypes.is_isomorphic (lt0, [(#"B", 2), (#"A", 1), (#"T", 1)]));

val _ = assert "nice_format groups equal factors" (LieTypes.nice_format [(#"A", 1), (#"A", 1), (#"B", 2)] = "2A1+B2");
val _ = assert "nice_format prints pure torus without leading +"
               (LieTypes.nice_format [(#"T", 2)] = "2T1");

val _ = assert "simple_type(F4) returns F4" (LieTypes.simple_type LieTypes.F4 = (#"F", 4));

val _ = print "ok\n";

