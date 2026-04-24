use "atlas-scripts-sml/genuine.sml";
use "atlas-scripts-sml/KType.sml";

(*
  Smoke test for `atlas-scripts-sml/genuine.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val () = assert "param_trivial ok" (p <> Foreign.Memory.null);

(* ParamPol positivity checks. *)
val poly = ParamPol.create ();
val () = ParamPol.addTermClone (poly, Split.one_plus_s, p);
val () = assert "is_positive_parampol" (Genuine.is_positive_parampol poly);

val poly2 = ParamPol.create ();
val () = ParamPol.addTermClone (poly2, Split.neg Split.one_plus_s, p);
val () = assert "is_positive_or_negative_parampol" (Genuine.is_positive_or_negative_parampol poly2);

val (bad, ok) = Genuine.test_positive_parampol poly2;
val () = assert "test_positive finds bad term" (not ok andalso not (ParamPol.isEmpty bad));
val () = ParamPol.free bad;

val () = ParamPol.free poly2;
val () = ParamPol.free poly;

(* KTypePol sign utilities: just ensure they run and return non-null when normalizing. *)
val t = KType.ofParam p;
val pol = KType.K_type_formula (t, 6);
val () = assert "K_type_formula ok" (pol <> Foreign.Memory.null);
val polN = Genuine.coeff_normalize_ktypepol pol;
val () = assert "coeff_normalize_ktypepol ok" (polN <> Foreign.Memory.null);
val _ = KTypePol.free polN;
val _ = KTypePol.free pol;
val _ = KType.free t;

val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "test_genuine_smoke: ok\n";
