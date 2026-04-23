use "atlas-scripts-sml/complex.sml";
use "atlas-scripts-sml/groups.sml";

(*
  Smoke test for the partial `complex.at` port (`atlas-scripts-sml/complex.sml`).
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

(* Pure slicing / corner extraction. *)
val m2 = [[0, 2], [3, 0]];
val () = assert "up_right_corner([[0,2],[3,0]]) = [[2]]" (Complex.up_right_corner m2 = [[2]]);
val () = assert "up_left_corner([[0,2],[3,0]]) = [[0]]" (Complex.up_left_corner m2 = [[0]]);

(* RootDatum-level complex test: A1 x A1 with delta swapping factors. *)
val a1 = RootDatum.newSimple (#"A", 1, false);
val a1b = RootDatum.newSimple (#"A", 1, false);
val rd = RootDatum.mul (a1, a1b);
val () = RootDatum.free a1;
val () = RootDatum.free a1b;

val deltaSwap = [[0, 1], [1, 0]];
val deltaId = [[1, 0], [0, 1]];
val () = assert "A1xA1 swap is complex" (Complex.is_complex_rootdatum (rd, deltaSwap));
val () = assert "A1xA1 identity is not complex" (not (Complex.is_complex_rootdatum (rd, deltaId)));
val () = RootDatum.free rd;

(* Group-level complex test: simple A1 real form should not be complex. *)
val g = Groups.quasisplit (#"A", 1, #"s");
val () = assert "A1_s is not complex (weakly)" (not (Complex.is_complex_group g));
val () = AtlasFFI.atlas_group_free g;

val _ = print "ok\n";

