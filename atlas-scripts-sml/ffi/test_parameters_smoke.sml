use "atlas-scripts-sml/parameters.sml";
use "atlas-scripts-sml/groups.sml";

(*
  Smoke test for `atlas-scripts-sml/parameters.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = Groups.quasisplit (#"A", 1, #"s"); (* SL(2,R) *)
val () = assert "square(g) has correct length"
               (length (#nums (Parameters.square g)) = AtlasFFI.atlas_group_rank g);

val kgbSize = AtlasFFI.atlas_group_kgb_size g;
val () = assert "kgb size positive" (kgbSize > 0);
val x0 = 0;

val theta0 = Parameters.theta_matrix (g, x0);
val (nRows, nCols) = Lattice.matShape theta0;
val () = assert "theta(x) is square"
               (nRows = AtlasFFI.atlas_group_rank g andalso nCols = AtlasFFI.atlas_group_rank g);

val tf0 = Parameters.torus_factor (g, x0);
val utf0 = Parameters.unnormalized_torus_factor (g, x0);
val () = assert "torus_factor length"
               (length (#nums tf0) = AtlasFFI.atlas_group_rank g);
val () = assert "unnormalized_torus_factor length"
               (length (#nums utf0) = AtlasFFI.atlas_group_rank g);

val () = assert "square_is_central holds at x=0" (Parameters.square_is_central (g, x0));

val p0 = AtlasFFI.atlas_param_trivial g;
val () = assert "trivial param allocated" (p0 <> Foreign.Memory.null);

val p1 = Parameters.contragredient p0;
val () = assert "contragredient allocated" (p1 <> Foreign.Memory.null);
val () = assert "contragredient(trivial)=trivial"
               (AtlasFFI.atlas_param_equal (p0, p1) = 1);

val () = AtlasFFI.atlas_param_free p1;
val () = AtlasFFI.atlas_param_free p0;
val () = AtlasFFI.atlas_group_free g;

val _ = print "ok\n";
