(* Smoke test for `atlas-scripts-sml/standardize.sml`. *)
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Split.sml";
use "atlas-scripts-sml/ParamPol.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/standardize.sml";

val g = AtlasFFI.atlas_group_new_F4_s ();
val () = if g = Foreign.Memory.null then raise Fail ("group_new_F4_s failed: " ^ AtlasFFI.atlas_last_error ()) else ();

val p = Representations.trivial g;
val poly = ParamPol.create ();
val () = ParamPol.addTermMove (poly, Split.one, p);

val poly2 = Standardize.coherent_simple_reflect (0, poly);
val () =
  print
    ( "coherent_simple_reflect terms: "
      ^ Int.toString (length (ParamPol.terms poly2))
      ^ "\n"
    );

val kgp = Standardize.KGP_set (g, 0);
val () = print ("KGP_set size: " ^ Int.toString (length kgp) ^ "\n");

val () = (ParamPol.free poly; ParamPol.free poly2; AtlasFFI.atlas_group_free g);

