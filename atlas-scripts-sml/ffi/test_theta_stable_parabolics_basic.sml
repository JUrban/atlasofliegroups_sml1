use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/parabolics.sml";

fun assertTrue (b: bool, msg: string) = if b then () else raise Fail msg;

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val tw = Parabolics.twist g;
val tsp = Parabolics.theta_stable_parabolics g;
val () = assertTrue (not (null tsp), "theta_stable_parabolics: empty");

val () =
  List.app
    (fn (S, y) =>
      ( assertTrue (Parabolics.is_twist_stable_subset tw S, "theta_stable_parabolics: S not twist-stable")
      ; assertTrue (Parabolics.is_closed g (S, y), "theta_stable_parabolics: orbit not closed")
      ))
    tsp;

val x = 0;
val tspWith = Parabolics.theta_stable_parabolics_with g x;
val () =
  List.app
    (fn P =>
      assertTrue
        (List.exists (fn Q => Q = P) tsp, "theta_stable_parabolics_with: element not in global list"))
    tspWith;

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

