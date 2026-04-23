use "atlas-scripts-sml/sp4.sml";

(*
  Smoke test for `atlas-scripts-sml/sp4.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val () = assert "sp4R is allocated" (Sp4.sp4R <> Foreign.Memory.null);
val () = assert "sp4 rootdatum is allocated" (Sp4.sp4 <> Foreign.Memory.null);

val () = assert "rank(sp4R)=2" (AtlasFFI.atlas_group_rank Sp4.sp4R = 2);
val () = assert "kgbSize positive" (Sp4.kgbSize > 0);

val p = Sp4.ds0 (2, 1);
val () = assert "ds0 param allocated" (p <> Foreign.Memory.null);
val () = AtlasFFI.atlas_param_free p;

val q = Sp4.c3 (0, 1, Rat.make (~1, 2), Rat.make (3, 2));
val () = assert "c3 param allocated" (q <> Foreign.Memory.null);
val () = AtlasFFI.atlas_param_free q;

val t = Sp4.triv ();
val () = assert "triv allocated" (t <> Foreign.Memory.null);
val () = AtlasFFI.atlas_param_free t;

val _ = print "ok\n";
