use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/parameters.sml";
use "atlas-scripts-sml/translate.sml";

(*
  Smoke test for the ported subset of `translate.at`:
  - `Translate.translate_param_by`
  - `Translate.T_param`
*)

fun assert msg b = if b then () else raise Fail ("assertion failed: " ^ msg);

fun ratvecAddIntShift (u: Lattice.ratvec, shift: int list) : Lattice.ratvec =
  let
    val u = Lattice.ratvecNormalize u
    val den = #den u
    val nums = #nums u
    val () = if length nums = length shift then () else raise Fail "ratvecAddIntShift: length mismatch"
    val nums2 = ListPair.mapEq (fn (a, s) => a + s * den) (nums, shift)
  in
    Lattice.ratvecNormalize {den = den, nums = nums2}
  end;

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val () = assert "param_trivial ok" (p <> Foreign.Memory.null);

val shift = [1, 0, 0, 0];

val gamma0 = Parameters.parseRatvecText (AtlasFFI.atlas_param_gamma_text p);
val gamma1 = ratvecAddIntShift (gamma0, shift);

val p2 = Translate.translate_param_by (p, shift);
val () = assert "translate_param_by ok" (p2 <> Foreign.Memory.null);
val () = assert "gamma shifted" (Parameters.parseRatvecText (AtlasFFI.atlas_param_gamma_text p2) = gamma1);

val p3 = Translate.T_param (p, gamma1);
val () = assert "T_param ok" (p3 <> Foreign.Memory.null);
val () = assert "T_param hits target gamma" (Parameters.parseRatvecText (AtlasFFI.atlas_param_gamma_text p3) = gamma1);

val _ = AtlasFFI.atlas_param_free p3;
val _ = AtlasFFI.atlas_param_free p2;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "test_translate_smoke: ok\n";

