use "atlas-scripts-sml/F4_FPP_points_seed.sml";

(*
  Smoke test for `F4_FPP_points_seed`: loads the 1864-point fixture into a
  `BigUnitaryHashStore` and checks the resulting hash size.
*)

val {g, store, ...} = F4_FPP_points_seed.seed_F4_s_store ();
val uhash = BigUnitaryHashStore.uhash store g;

val () = if ParamHash.size uhash = 1864 then () else raise Fail "expected 1864 points";

val () = BigUnitaryHashStore.freeAll store;
val () = AtlasFFI.atlas_group_free g;

val () = TextIO.print "OK\n";

