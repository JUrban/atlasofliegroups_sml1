use "atlas-scripts-sml/restricted_roots.sml";
use "atlas-scripts-sml/groups.sml";

(*
  Smoke test for `atlas-scripts-sml/restricted_roots.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

(* Split case: restricted roots should coincide with the full root system. *)
val g = Groups.Sp_R 4; (* type C2 split *)
val rdRes = RestrictedRoots.restricted_roots_short g;
val lt = RootDatum.lieType rdRes;
val () = assert "restricted_roots_short(Sp(4,R)) has type C2" (lt = [(#"C", 2)]);
val () = RootDatum.free rdRes;
val () = AtlasFFI.atlas_group_free g;

val _ = print "ok\n";

