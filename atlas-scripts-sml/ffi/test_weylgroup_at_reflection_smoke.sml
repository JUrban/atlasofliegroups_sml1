use "atlas-scripts-sml/weylgroup_at.sml";

(*
  Smoke test for `atlas-scripts-sml/weylgroup_at.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rdA1 = RootDatum.newSimple (#"A", 1, false);

val r = WeylgroupAT.reflection_matrix_simple (rdA1, 0);
val _ = assert "A1 reflection matrix is [-1]" (r = [[~1]]);

val v = [3];
val v2 = WeylgroupAT.reflect_simple (rdA1, 0, v);
val _ = assert "A1 reflect_simple negates" (v2 = [~3]);

val id = IntMatrix.identity 1;
val _ = assert "lengthens(id) for simple root" (WeylgroupAT.lengthens (rdA1, id, 0));
val _ = assert "lengthens(reflection) is false" (not (WeylgroupAT.lengthens (rdA1, r, 0)));

val rr = WeylgroupAT.right_reflect (rdA1, id, 0);
val _ = assert "right_reflect(id)=reflection" (rr = r);

val cc = WeylgroupAT.conjugate (rdA1, 0, id);
val _ = assert "conjugate(id)=id" (cc = id);

val () = RootDatum.free rdA1;
val _ = print "ok\n";
