use "atlas-scripts-sml/tits.sml";

(*
  Smoke test for `atlas-scripts-sml/tits.sml` against basic A1 identities.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rdA1 = RootDatum.newSimple (#"A", 1, false);

val e = Tits.tits_identity rdA1;
val s = Tits.right_simple (e, 0); (* σ_0 *)
val s2 = Tits.multiply (s, s);

val _ = assert "sigma^2 has theta=I" (#theta s2 = IntMatrix.identity 1);
val _ = assert "sigma^2 has torus_part=coroot/2 mod 1"
               (#torus_part s2 = Lattice.ratvecNormalize {den = 2, nums = [1]});

val sInv = Tits.inverse s;
val prod = Tits.multiply (s, sInv);
val _ = assert "s * s^{-1} = e (theta)" (#theta prod = #theta e);
val _ = assert "s * s^{-1} = e (torus)" (#torus_part prod = #torus_part e);

val () = RootDatum.free rdA1;
val _ = print "ok\n";

