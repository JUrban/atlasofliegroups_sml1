use "atlas-scripts-sml/K_types.sml";

(*
  Smoke test for the ported subset of `atlas-scripts/K_types.at`:
  - FFI `branch` support for `KTypePol`
  - `K_types.K_signature_irr`
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val () = assert "param_trivial ok" (p <> Foreign.Memory.null);

val t = KType.ofParam p;
val rank = AtlasFFI.atlas_group_rank g;

val pol = KTypePol.singleton (t, 1, 2);
val ts = KTypePol.terms (pol, rank);
val () = assert "singleton has 1 term" (length ts = 1);
val () =
  (case ts of
     [u] => assert "singleton coef e/s" (#e u = 1 andalso #s u = 2)
   | _ => raise Fail "unexpected term list");

val polI = KTypePol.intPart pol;
val polS = KTypePol.sPart pol;
val () =
  (case KTypePol.terms (polI, rank) of
     [u] => assert "intPart coef" (#e u = 1 andalso #s u = 0)
   | _ => raise Fail "unexpected intPart term list");
val () =
  (case KTypePol.terms (polS, rank) of
     [u] => assert "sPart coef" (#e u = 2 andalso #s u = 0)
   | _ => raise Fail "unexpected sPart term list");

val bran = KTypePol.branch (pol, KType.height t);
val () = assert "branch non-null" (bran <> Foreign.Memory.null);

val (sigP, sigQ) = K_types.K_signature_irr (p, 6);
val () = assert "K_signature_irr P non-null" (sigP <> Foreign.Memory.null);
val () = assert "K_signature_irr Q non-null" (sigQ <> Foreign.Memory.null);

val _ = KTypePol.free sigQ;
val _ = KTypePol.free sigP;
val _ = KTypePol.free bran;
val _ = KTypePol.free polS;
val _ = KTypePol.free polI;
val _ = KTypePol.free pol;
val _ = KType.free t;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "test_K_types_smoke: ok\n";

