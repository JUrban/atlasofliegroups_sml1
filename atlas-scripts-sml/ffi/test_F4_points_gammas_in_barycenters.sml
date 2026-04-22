use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/FPP_barycenters_fold.sml";
use "atlas-scripts-sml/F4_FPP_points.sml";
use "atlas-scripts-sml/ParamHash.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";
use "atlas-scripts-sml/Lattice.sml";

fun key (u: Lattice.ratvec) : int list =
  let
    val u = Lattice.ratvecNormalize u
  in
    #den u :: #nums u
  end

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);

val barys = FPP_barycenters_fold.barycenters_all g;
val baryKeys = List.map key (Basic.sort_u_by (key, Sort.rlex_leq) barys);
val locate = Basic.binary_search_in (baryKeys, Sort.rlex_leq);

val expected = ParamHash.create 4096;
val () = F4_FPP_points.loadIntoParamHash (g, expected);
val ps = ParamHash.list expected;

fun gammaKey p = key (AllParameters.parseRatWeightText (AtlasFFI.atlas_param_gamma_text p));

val missing = List.filter (fn p => not (Option.isSome (locate (gammaKey p)))) ps;

val () =
  if null missing then
    print "OK\n"
  else
    raise Fail ("missing barycenters for " ^ Int.toString (length missing) ^ " / " ^ Int.toString (length ps));

val () = ParamHash.freeAll expected;
val () = AtlasFFI.atlas_group_free g;
